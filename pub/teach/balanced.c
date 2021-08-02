#include <stdio.h>

/* definition of a tree data structure */
struct tree {
  int value;
  struct tree *left;
  struct tree *right;
};

/* calculate the size of a tree */
int size (struct tree *t)
{
  if (t==NULL)
    return 0;
  else 
    return 1 + size(t->left) + size(t->right);
}

/* decide whether a tree is balanced */
int balanced (struct tree *t)
{
  int n,m;
  if (t==NULL)
    return 1;
  else {
    n = size (t->left);
    m = size (t->right);
    if (1/3 <= n/(n+m+1) && n/(n+m+1) <= 2/3)
      return balanced(t->left) && balanced (t->right);
    else 
      return 0;
  }
}

/* allocate a memory for tree node */
struct tree *new()
{
  return (struct tree *) malloc(sizeof(struct tree));
}

/* generate a small tree for test */
struct tree *genTree()
{
  struct tree *t,*t1,*t2;     /* creat a tree:  */
  t = new ();                 /*      1         */
  t1 = new (); t2=new ();     /*     /  \       */
  t->value = 1;               /*    -    2      */
  t->left  = NULL;            /*        / \     */
  t->right = t1;              /*       3   -    */
  t1->value = 2;              /*      / \       */
  t1->left = t2;              /*     -   -      */
  t1->right = NULL;
  t2->value = 3;
  t2->left = NULL;
  t2->right = NULL;
  return t;
}

/* free memory */
void freeTree(struct tree *t)
{
  if (t != NULL) {
    freeTree(t->left);
    freeTree(t->right);
  }
  free(t);
}

/* main program */
main()
{
  struct tree *t;
  t = genTree();
  if (balanced(t))
    printf("The given tree is balanced.\n");
  else
    printf("The given tree is unbalanced.\n");
  freeTree(t);
}


