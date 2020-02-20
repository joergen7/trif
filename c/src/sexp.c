#include "sexp.h"
#include <stdlib.h>

t_sexp* new_cell_array( t_idx size ) {

  t_sexp* ptr = malloc( sizeof( t_sexp )*size );
  
  if( ptr == NULL )
    exit( 1 );

  for( int i = 0; i < size; i++ )
    ptr[ i ].nref = 0;

  return ptr;
}

t_sexp* populate_cell( t_sexp* cell_array, t_idx size ) {

  for( int i = 0; i < size; i++ )
    if( cell_array[ i ].nref == 0 ) {
      cell_array[ i ].nref = 1;
      return &cell_array[ i ];
    }

  exit( 1 );
}

t_sexp* nil( t_sexp* cell_array, t_idx size ) {
  
  t_sexp* cell = populate_cell( cell_array, size );
  cell.type = Nil;

  return cell;
}