



// s-expression type

enum e_type { Nil, Cons, Sym, Bool, Int, Str };
typedef enum e_type t_type;

enum e_bool { true, false };
typedef enum e_bool t_bool;

typedef long long t_int;

typedef unsigned int t_idx;

// symbol

typedef unsigned int t_sym;


// cons and s-expression

struct s_cons {
  struct s_sexp* car;
  struct s_sexp* cdr;
};

union u_data {
  t_sym sym;
  struct s_cons cons;
};

struct s_sexp {
  t_type type;
  t_idx nref;
  union u_data data;
};

typedef struct s_cons t_cons;
typedef union u_data t_data;
typedef struct s_sexp t_sexp;


t_sexp* new_cell_array( t_idx size );
t_sexp* populate_cell( t_sexp* cell_array, t_idx size );

t_sexp* nil( t_sexp* cell_array, t_idx size );
t_sexp* cons( t_sexp* cell_array, t_idx size, t_sexp* car, t_sexp* cdr );
t_sexp* sym( t_sexp* cell_array, t_idx size, t_sym name );
t_sexp* boolean( t_sexp* cell_array, t_idx size, t_bool b );
t_sexp* integer( t_sexp* cell_array, t_idx size, t_int i );
t_sexp* str( t_sexp* cell_array, t_idx size, char* s );

// TODO: function to get pointer to next unused cell
// TODO: function to reduce reference count inline
// TODO: function to increase reference count inline