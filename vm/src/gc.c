#include <jelly/gc.h>

#include <jelly/vm.h>

#include <jelly/list.h>
#include <jelly/array.h>
#include <jelly/object.h>
#include <jelly/function.h>
#include <jelly/bytes.h>
#include <jelly/abstract.h>
#include <jelly/box.h>

#include <stdlib.h>
#include <string.h>

typedef struct jelly_gc_hdr {
  struct jelly_gc_hdr* next;
  size_t total_size;
  uint8_t marked;
  uint8_t _pad[7];
} jelly_gc_hdr;

static void destroy_object(void* p);

static jelly_gc_hdr* hdr_of(void* obj) {
  return (jelly_gc_hdr*)((uint8_t*)obj - sizeof(jelly_gc_hdr));
}

static void* obj_of(jelly_gc_hdr* h) {
  return (void*)((uint8_t*)h + sizeof(jelly_gc_hdr));
}

static void gc_panic(void) {
  abort();
}

void jelly_gc_init(struct jelly_vm* vm) {
  if(!vm) return;
  if(vm->gc_next_collect == 0) vm->gc_next_collect = 1024u * 1024u; // 1MB default
}

void jelly_gc_shutdown(struct jelly_vm* vm) {
  if(!vm) return;
  // Free everything on the GC heap (best-effort).
  jelly_gc_hdr* h = (jelly_gc_hdr*)vm->gc_objects;
  while(h) {
    jelly_gc_hdr* next = h->next;
    destroy_object(obj_of(h));
    free(h);
    h = next;
  }
  vm->gc_objects = NULL;
  vm->gc_bytes_live = 0;

  free(vm->gc_roots);
  vm->gc_roots = NULL;
  vm->gc_roots_len = 0;
  vm->gc_roots_cap = 0;
}

void jelly_gc_push_root(struct jelly_vm* vm, jelly_value v) {
  if(!vm) gc_panic();
  if(vm->gc_roots_len == vm->gc_roots_cap) {
    uint32_t ncap = vm->gc_roots_cap ? (vm->gc_roots_cap * 2u) : 32u;
    jelly_value* nv = (jelly_value*)realloc(vm->gc_roots, sizeof(jelly_value) * (size_t)ncap);
    if(!nv) gc_panic();
    vm->gc_roots = nv;
    vm->gc_roots_cap = ncap;
  }
  vm->gc_roots[vm->gc_roots_len++] = v;
}

void jelly_gc_pop_roots(struct jelly_vm* vm, uint32_t n) {
  if(!vm) gc_panic();
  if(n > vm->gc_roots_len) gc_panic();
  vm->gc_roots_len -= n;
}

// --- mark ---------------------------------------------------------------------

typedef struct mark_stack {
  void** items;
  uint32_t len;
  uint32_t cap;
} mark_stack;

static void ms_push(mark_stack* ms, void* p) {
  if(ms->len == ms->cap) {
    uint32_t ncap = ms->cap ? (ms->cap * 2u) : 64u;
    void** ni = (void**)realloc(ms->items, sizeof(void*) * (size_t)ncap);
    if(!ni) gc_panic();
    ms->items = ni;
    ms->cap = ncap;
  }
  ms->items[ms->len++] = p;
}

static void* ms_pop(mark_stack* ms) {
  if(ms->len == 0) return NULL;
  return ms->items[--ms->len];
}

static void mark_ptr(mark_stack* ms, void* p) {
  if(!p) return;
  jelly_gc_hdr* h = hdr_of(p);
  if(h->marked) return;
  h->marked = 1;
  ms_push(ms, p);
}

static void mark_val(mark_stack* ms, jelly_value v) {
  if(jelly_is_ptr(v)) {
    mark_ptr(ms, jelly_as_ptr(v));
  }
}

static void trace_object(mark_stack* ms, void* p) {
  const jelly_obj_header* oh = (const jelly_obj_header*)p;
  switch((jelly_obj_kind)oh->kind) {
    case JELLY_OBJ_BYTES:
    case JELLY_OBJ_BOX_I64:
    case JELLY_OBJ_BOX_F64:
    case JELLY_OBJ_BOX_F32:
      return;
    case JELLY_OBJ_LIST: {
      const jelly_list* n = (const jelly_list*)p;
      mark_val(ms, n->head);
      mark_ptr(ms, n->tail);
      return;
    }
    case JELLY_OBJ_ARRAY: {
      const jelly_array* a = (const jelly_array*)p;
      for(uint32_t i = 0; i < a->length; i++) mark_val(ms, a->data[i]);
      return;
    }
    case JELLY_OBJ_OBJECT: {
      const jelly_object* o = (const jelly_object*)p;
      mark_ptr(ms, (void*)o->proto);
      for(uint32_t i = 0; i < o->cap; i++) {
        if(o->states[i] == 1) mark_val(ms, o->vals[i]);
      }
      return;
    }
    case JELLY_OBJ_FUNCTION: {
      const jelly_function* f = (const jelly_function*)p;
      mark_val(ms, f->bound_this);
      for(uint32_t i = 0; i < f->ncaps; i++) mark_val(ms, f->caps[i]);
      return;
    }
    case JELLY_OBJ_ABSTRACT:
      // Opaque by default.
      return;
    default:
      return;
  }
}

// --- sweep --------------------------------------------------------------------

static void destroy_object(void* p) {
  const jelly_obj_header* oh = (const jelly_obj_header*)p;
  switch((jelly_obj_kind)oh->kind) {
    case JELLY_OBJ_ARRAY: {
      jelly_array* a = (jelly_array*)p;
      free(a->data);
      a->data = NULL;
      a->length = 0;
      return;
    }
    case JELLY_OBJ_OBJECT: {
      jelly_object* o = (jelly_object*)p;
      free(o->keys);
      free(o->vals);
      free(o->states);
      o->keys = NULL;
      o->vals = NULL;
      o->states = NULL;
      return;
    }
    case JELLY_OBJ_FUNCTION: {
      // Single-allocation closure (captures are inline).
      return;
    }
    case JELLY_OBJ_ABSTRACT: {
      jelly_abstract* a = (jelly_abstract*)p;
      if(a->finalizer) a->finalizer(a->payload);
      a->payload = NULL;
      a->finalizer = NULL;
      return;
    }
    default:
      return;
  }
}

// Root scanning helpers.
static void scan_typed_frames(struct jelly_vm* vm, mark_stack* ms) {
  const jelly_bc_module* m = vm->running_module;
  if(!m) return;
  // call_frames is managed by exec.c; we only depend on the common prefix layout:
  // struct call_frame { const jelly_bc_function* f; reg_frame rf; ... }
  // reg_frame is { uint8_t* mem; uint32_t* off; uint32_t nregs; }
  typedef struct reg_frame_view { uint8_t* mem; uint32_t* off; uint32_t nregs; } reg_frame_view;
  typedef struct call_frame_view { const jelly_bc_function* f; reg_frame_view rf; } call_frame_view;

  const call_frame_view* frames = (const call_frame_view*)vm->call_frames;
  for(uint32_t fi = 0; fi < vm->call_frames_len; fi++) {
    const jelly_bc_function* f = frames[fi].f;
    const reg_frame_view* rf = &frames[fi].rf;
    for(uint32_t r = 0; r < rf->nregs; r++) {
      jelly_type_kind k = m->types[f->reg_types[r]].kind;
      if(k == JELLY_T_DYNAMIC) {
        jelly_value v;
        memcpy(&v, rf->mem + rf->off[r], sizeof(v));
        mark_val(ms, v);
      } else {
        switch(k) {
          case JELLY_T_BYTES:
          case JELLY_T_FUNCTION:
          case JELLY_T_LIST:
          case JELLY_T_ARRAY:
          case JELLY_T_OBJECT:
          case JELLY_T_ABSTRACT: {
            void* p = NULL;
            memcpy(&p, rf->mem + rf->off[r], sizeof(void*));
            mark_ptr(ms, p);
            break;
          }
          default:
            break;
        }
      }
    }
  }
}

void jelly_gc_collect(struct jelly_vm* vm) {
  if(!vm) gc_panic();

  vm->gc_collections++;

  mark_stack ms = {0};

  // Roots: spill
  for(uint32_t i = 0; i < vm->spill_len; i++) mark_val(&ms, vm->spill[i]);
  // Roots: temp roots
  for(uint32_t i = 0; i < vm->gc_roots_len; i++) mark_val(&ms, vm->gc_roots[i]);
  // Roots: typed call frames
  scan_typed_frames(vm, &ms);

  // Mark loop
  for(;;) {
    void* p = ms_pop(&ms);
    if(!p) break;
    trace_object(&ms, p);
  }
  free(ms.items);

  // Sweep
  jelly_gc_hdr** prev = (jelly_gc_hdr**)&vm->gc_objects;
  jelly_gc_hdr* h = (jelly_gc_hdr*)vm->gc_objects;
  size_t live_bytes = 0;
  while(h) {
    jelly_gc_hdr* next = h->next;
    if(!h->marked) {
      void* obj = obj_of(h);
      destroy_object(obj);
      vm->gc_freed_objects++;
      *prev = next;
      free(h);
    } else {
      h->marked = 0;
      live_bytes += h->total_size;
      prev = &h->next;
    }
    h = next;
  }

  vm->gc_bytes_live = live_bytes;
  // Next threshold: grow by ~2x with a floor.
  vm->gc_next_collect = (live_bytes < 256u * 1024u) ? (512u * 1024u) : (live_bytes * 2u);
}

void* jelly_gc_alloc(struct jelly_vm* vm, size_t size) {
  if(!vm) gc_panic();

  if(vm->gc_next_collect && vm->gc_bytes_live >= vm->gc_next_collect) {
    jelly_gc_collect(vm);
  }

  size_t total = sizeof(jelly_gc_hdr) + size;
  jelly_gc_hdr* h = (jelly_gc_hdr*)malloc(total);
  if(!h) gc_panic();
  h->next = (jelly_gc_hdr*)vm->gc_objects;
  h->total_size = total;
  h->marked = 0;
  vm->gc_objects = h;
  vm->gc_bytes_live += total;
  return obj_of(h);
}

