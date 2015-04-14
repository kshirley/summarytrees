#include <R.h>
#include <Rmath.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <assert.h>
#include <time.h>
//#define MAXN 50000000
//#define MAXNUMCHILDREN MAXN
#define INFTY 1.0e30
#define DEBUG 0
//#define MAXK 500
#define INTINFTY 444444444
#define LOGBASE 2.0

// Some global variable declarations:
int *childstart;
int *childend;
int ***rightcount;
int **indexa;
int *numchildrenof;
float **F;
double *size;
double *weight;
double entropy;

// Some functions for maximum entropy summary trees:
void drawSTofchildren(int v, int l, int h);
void drawSTrootedatonevertex(int v, int h);
double xlogx(double x);
float combine(double h1, double w1, double h2, double w2);

// local version of heapsort:
void heapify(double *a, int *indexarray, long i, long n);
void sort(double *a, int *indexarray, long n);
void buildheap(double *a, int *indexarray, long n);
void heapsort_howard(double *a, int *indexarray, long n);


////////////////////////////////////////////////////////////

void Rgreedy(int *R_K, int *R_n, int *R_numparents, 
	     double *R_weight, int *R_childindex, int *R_childstart, 
	     int *R_childend) {

  int K, n, numparents;
  int i, j, k, v, d, k1, k2;
  int *childindex, *parent;
  int *depth, *minnodeofdepth, *maxnodeofdepth;
  int maxdepth;
  double *oldarray, *newarray, *temparray;
  int maxnumchildren;
  double *a;
  int l, vell, svl;
  double sumofsizesofchildrenuptolminus1;
  double temp;
  int jjj;
  double maxentv;
  double sumofsizesofallchildren;

  // set values of scalars from input:
  K = R_K[0];
  n = R_n[0];
  numparents = R_numparents[0];

  // allocate space for several variables:
  childindex = (int *) malloc ((numparents + 1)*sizeof(int));
  childstart = (int *) malloc ((n + 1)*sizeof(int));
  childend = (int *) malloc ((n + 1)*sizeof(int));
  weight = (double *) malloc ((n + 1)*sizeof(double));
  size = (double *) malloc ((n + 1)*sizeof(double));
  parent = (int *) malloc ((n + 1)*sizeof(int));
  depth = (int *) malloc ((n + 1)*sizeof(int));
  minnodeofdepth = (int *) malloc ((n + 1)*sizeof(int));
  maxnodeofdepth = (int *) malloc ((n + 1)*sizeof(int));
  oldarray = (double *) malloc ((K + 1)*sizeof(double));
  newarray = (double *) malloc ((K + 1)*sizeof(double));
  temparray = (double *) malloc ((K + 1)*sizeof(double));
  numchildrenof = (int *) malloc ((n + 1)*sizeof(int));  
  indexa = (int **) malloc ((n + 1)*sizeof(int *));

  // Malloc F (an [n, K] matrix):
  F = (float **) malloc ((1 + n)*sizeof(float *));
  for (i = 0; i <= n; ++i) {
    F[i] = (float *) malloc((1 + K)*sizeof(float));
  }
    
  // gather node indices of nodes that have > 0 children from input:
  for (i = 1; i <= numparents; ++i) {
    childindex[i] = R_childindex[i - 1];
    //Rprintf("i = %d and childindex[%d] = %d\n", i, i, childindex[i]);
  }

  // initialize the childstart and childend to be zero for all nodes:
  for (i = 0; i <= n; ++i) {
    childstart[i] = childend[i] = 0;
  }
  
  // fill childstart and childend from input:
  for (i = 1; i <= numparents; ++i) {
    childstart[childindex[i]] = R_childstart[i - 1];
    childend[childindex[i]] = R_childend[i - 1];
  }

  // fill weight from input:
  for (i = 1; i <= n; ++i) {
    weight[i] = R_weight[i - 1];
    //if ((i <= 10) || (i == n)) Rprintf("weight[%d]=%lf.\n", i, weight[i]);
  }

  // Compute the size of each node
  // The size of node v is the sum of weight[x] over descendants x of v
  for (i = n; i >= 1; --i) {
    if (childstart[i] == 0) size[i] = weight[i];
    else {
      size[i] = weight[i]; // This is initialization only.
      for (j = childstart[i]; j <= childend[i]; ++j) {
	size[i] += size[j];
      }
      //if (i <= 1000) Rprintf("size[%d]=%lf.\n", i, size[i]);
    }
  }

  // Compute parent pointers:
  parent[1] = 0; // root has no parent.
  for (i = 1; i <= n; ++i) {
    if (childstart[i] != 0) {
      for (j = childstart[i]; j <= childend[i]; ++j) {
	parent[j] = i;
      }
    }
  }

  // Now compute nodes' depths.
  for (v = 1; v <= n; ++v) {
    if (v == 1) depth[v] = 0;
    else {
      d = depth[v] = 1 + depth[parent[v]];
      if (d > depth[v - 1]) {
	minnodeofdepth[d] = v;
	maxnodeofdepth[d - 1] = v - 1;
      }
    }
  }
  maxdepth = depth[n];
  //Rprintf("maxdepth=%d.\n", maxdepth);
  minnodeofdepth[0] = 1; maxnodeofdepth[0] = 1;
  maxnodeofdepth[maxdepth] = n;

  // Print range of indices of nodes at each depth:
  /* for (d = 0; d <= maxdepth; ++d) { */
  /*   Rprintf("minnodeofdepth[%d]=%d; maxnodeofdepth[%d]=%d.\n", */
  /* 	    d, minnodeofdepth[d], d, maxnodeofdepth[d]); */
  /* } */


    
    
  //setvbuf(stdout, 0, _IONBF, 0); setvbuf(stderr, 0, _IONBF, 0);
    
  // Nodes will be labeled in increasing order by depth, with the root given label 1.
  // The children of v, if any, will be childstart[v], childstart[v]+1,..., childend[v].
  // If v has no children, then childstart[v]=childend[v]=0.
        
  // The nodes are labeled in increasing order by depth.  Therefore,
  // we will process them in decreasing order by depth, skipping the leaf nodes.
    
  // Now process one level of nodes at a time, in decreasing
  // order by level.  If a node is a leaf, we define the variables directly.
  // If not, we combine the values on its children to get the value for the node.
    
    
  // F_v[k] will be the entropy of some k-node summary tree for T_v.
  // Fill F:  
  for (i = 1; i <= n; ++i) {
    for (k = 1; k <= K; ++k) {
      F[i][k] = -INFTY;
    }
  }
  
  for (i = maxnodeofdepth[maxdepth]; i >= minnodeofdepth[maxdepth]; --i) {
    // Node v is a leaf, so only a 1-node summary tree of T_v is possible.
    if (0 > F[i][1]) F[i][1] = 0;
  }
    
  maxnumchildren = 0;
  for (v = 1; v <= n; ++v) {
    if (childend[v] == 0) {
      // Then v is a leaf.
      numchildrenof[v] = 0; // v is a leaf
    }
    else {
      numchildrenof[v] = childend[v] - childstart[v] + 1;
      if (numchildrenof[v] > maxnumchildren) maxnumchildren = numchildrenof[v];
    }
    indexa[v] = (int *) malloc ((1 + numchildrenof[v])*sizeof(int));
  }
  //Rprintf("%d is the max number of children\n", maxnumchildren);


  // Malloc rightcount:
  rightcount = (int ***) malloc ((n + 1)*sizeof(int **));
  for (v = 1; v <= n; ++v) {
    rightcount[v] = (int **) malloc ((1 + numchildrenof[v])*sizeof(int *));
    for (i = 1; i <= numchildrenof[v]; ++i) {
      rightcount[v][i] = (int *) malloc ((K + 1)*sizeof(int));
    }
  }

  // Size of rightcount is K*sum_v (1+numchildrenof[v]);
  // this is <=Kn+Kn.
  // Initialize rightcount
  for (i = 1; i <= n; ++i) {
    for (j = 1; j <= numchildrenof[i]; ++j) {
      for (k = 1; k <= K; ++k) {
	rightcount[i][j][k] = -INTINFTY;
      }
    }
  }

  // malloc a:
  a = (double *) malloc((1 + maxnumchildren)*sizeof(double));

  for (v = 1; v <= n; ++v) {
    if (childend[v] != 0) { // There is no indexa[v] array for leaves v.
      for (l = 1; l <= numchildrenof[v]; ++l) {
	vell = childstart[v] + (l- 1);
	a[l] = size[vell];
	indexa[v][l] = l;
      }
      heapsort_howard(a, indexa[v], numchildrenof[v]); // indexa[v] is a
      //heapsort(a, indexa[v], numchildrenof[v]); // indexa[v] is a
      // 1-d array, though indexa[][] is a 2-d array.
      /* if (DEBUG) { */
      /* 	Rprintf("\nNow printing indexa[%d]: ",v); */
      /* 	for (i=1; i<=numchildrenof[v]; ++i) { */
      /* 	  Rprintf("%d ",indexa[v][i]); */
      /* 	} */
      /* 	Rprintf("\n"); */
      /* } */
    }
  }


  // Loop through the levels of the tree and compute max entropy summary trees:
  // We will process the nodes in decreasing order
  // by depth, processing all nodes at depth d prior
  // to any at depth d-1.  This will keep our memory usage down.
  for (d = maxdepth - 1; d >= 0; --d) {
    // Process level d.
        
    for (v = maxnodeofdepth[d]; v >= minnodeofdepth[d]; --v) {
      if (v % 10000 == 0) Rprintf("Processing node %d.\n", v);
      if (childstart[v] == 0) { // i.e., v is a leaf
	if (0 > F[v][1]) F[v][1] = 0;
	sumofsizesofchildrenuptolminus1 = 0; // not used; just done to silence a compiler warning.
	numchildrenof[v] = 0; // the same
      }
            
      else { // Now process a nonleaf.
	sumofsizesofchildrenuptolminus1 = 0;
	// We will process the children of v in nondecreasing order by their sizes.
                
	for (k = 1; k <= K; ++k) {
	  oldarray[k] = F[childstart[v] + indexa[v][1] - 1][k];
	  // This is the
	  // first child of v in the sorted order.  It's the
	  // child of the least size.
	  //if (DEBUG) Rprintf("For v=%d, l=1, oldarray[%d]=%lf.\n", v, k, oldarray[k]);
	}
	for (l = 2; l <= numchildrenof[v]; ++l) {
	  sumofsizesofchildrenuptolminus1 += size[childstart[v] + indexa[v][l - 1] - 1];
	  // This is the (l-1)st child, in sorted order.
	  vell = childstart[v] + indexa[v][l] - 1;
	  svl = size[vell];
	  for (k = 1; k <= K; ++k) {
	    newarray[k] = F[vell][k];
	    temparray[k] = -INFTY;
	  }
	  // Now combine oldarray and newarray, to get solution
	  // for children 1,2,...,l.
                    
	  for (k1 = 1; k1 <= K; ++k1) {
	    for (k2 = 1; k2 <= K - k1; ++k2) {
	      k = k1 + k2;
	      temp = combine(oldarray[k1], sumofsizesofchildrenuptolminus1,
			     newarray[k2], svl);
	      if (temp > temparray[k]) {
		temparray[k] = temp;
		rightcount[v][l][k] = k2;
	      }
	    }
	  }
                    
	  if (DEBUG) {
	    for (k = 1; k <= K; ++k) {
	      Rprintf("rightcount[%d][%d][%d]=%d.\n",
	  	     v, l, k, rightcount[v][l][k]);
	    }
	  }

	  temparray[1] = 0; // We are creating one "other" node
	  // containing all nodes in the subtrees rooted at 
	  // v_1,v_2,...,v_l.
	  // Note:  This greedy algorithm allows "other" to be 
	  // the union of T_{v_1},T_{v_2},...,T_{v_l}, for some l,
	  // and no other possibilities;  this is why its output
	  // is not optimal.
	  for (k = 1; k <= K; ++k) {
	    oldarray[k] = temparray[k];
	    if (DEBUG) Rprintf("For node v=%d and l=%d, k=%d, entropy=%lf.\n", v, l, k, temparray[k]);
	  }
	} // for (l=2) loop
                
	// Now we have to "attach the root."
	jjj = childstart[v] + indexa[v][numchildrenof[v]] - 1;
	//if (jjj < 1) Rprintf("jjj=%d at v=%d.\n", jjj, v);
	//if (TRUE) Rprintf("jjj=%d at v=%d.\n", jjj, v);
	//assert(jjj >= 1);
	sumofsizesofallchildren = sumofsizesofchildrenuptolminus1 + size[childstart[v] + indexa[v][numchildrenof[v]] - 1];
      
	F[v][1] = 0;
	//if (v == 1) Rprintf("now v=%d: entropy of the %d-node summary tree is %lf.\n", 1, 1, 0.0);
	for (k = 2; k <= K; ++k) {
	  maxentv = F[v][k] = combine(0, weight[v], oldarray[k - 1], sumofsizesofallchildren);
	  //if (childstart[v] == 0) Rprintf("%d is a leaf and F[%d][%d]=%lf.\n", v, v, k, F[v][k]);
	  if (maxentv < 0) maxentv = 0; 
	  //if (v == 1) Rprintf("now v=%d: entropy of the %d-node summary tree that greedy found is %lf.\n", v, k, maxentv);
	}
      } // else (now process a nonleaf            
    } // "for v" loop
  } // for d=maxdepth-1 downward
    
  for (k = 1; k <= K; ++k) {
    Rprintf("\nNow printing the greedy ST found on %d nodes:\n", k);
    //if (DEBUG) Rprintf("Line 523:  about to call drawSTrootedatonevertex(1,%d).\n", k);
    entropy = 0;
    drawSTrootedatonevertex(1, k);
    //Rprintf("entropy=%lf.\n", entropy);
  }

} // main


// special function to handle log(0)
double xlogx(double x) {
  //assert(x >= 0);
  if (x == 0) return (0);
  else return (x*log(x)/log(LOGBASE));
}

////////////////////////////////////////////////////////////

float combine(double h1, double w1, double h2, double w2) {
  // In greedy, we allow
  // real weights, which we do not allow in the approximation code.
  double q, Hofq;
  assert(w1 >= 0);
  assert(w2 >= 0);
  if ((h1 <= -1.0e10) || (h2 <= -1.0e10)) return(-INFTY);
  if ((w1 == 0) && (w2 == 0)) return (0);
  assert(w1 + w2 >= 0);
  q=((float) w1)/((float) w1 + w2);
  assert((0 <= q) && (q <= 1));
  if ((q == 0) || (q == 1)) Hofq = 0;
  else Hofq = -(xlogx(q) + xlogx(1.0 - q));
  return (q*h1 + (1 - q)*h2 + Hofq);
}

////////////////////////////////////////////////////////////


void drawSTrootedatonevertex(int v, int h) {
  // "ST" = "summary tree"
  int dv; // "vdv"="v_{d_v}"
  //if (DEBUG) Rprintf("Line 121: drawSTrootedatonevertex(%d,%d) just called.\n", v, h);

  //assert(1 <= v);
  dv = numchildrenof[v];
  //if (dv == 0) assert(h == 1); // Can only draw 1-node ST of a 1-node tree!
  if (h == 1) {
    Rprintf("T_%d; weight of cluster=%lf.\n", v, size[v]);
    entropy -= xlogx(size[v]/size[1]);
    return;
  }
    
  // Now h>=2.
  Rprintf("{%d}; weight=%lf.\n", v, weight[v]);
  entropy -= xlogx(weight[v]/size[1]);
  drawSTofchildren(v, dv, h - 1);
}


////////////////////////////////////////////////////////////

void drawSTofchildren(int v, int l, int h) {
  // Draw an h-node ST of the union of the l subtrees rooted
  // at children of v of least size.
  double sum; int node;
    
  int i, v1, vell, r; // "vell" is read "v-ell"
    
  v1 = (childstart[v] - 1) + indexa[v][1]; // child of v of least size
  if (l == 1) {
    if (DEBUG) Rprintf("Line 177: about to call drawSTrootedatonevertex(%d,%d).\n",
		      v1,h);
    drawSTrootedatonevertex(v1, h);
    return;
  }
    
  vell = (childstart[v] - 1) + indexa[v][l];
  if (h == 1) {
    sum = 0;
    Rprintf("Other=");
    for (i = 1; i <= l; ++i) {
      if (DEBUG) Rprintf("Line 187: ");
      node = (childstart[v] - 1) + indexa[v][i];
      sum += size[node];
      Rprintf("T_%d", node);
      if (i < l) Rprintf(" union ");
    }
    Rprintf("; weight of cluster=%lf.\n",sum);
    entropy -= xlogx(sum/size[1]);
    return;
  }
    
  // Now h >= 2.
  r = rightcount[v][l][h];
  drawSTrootedatonevertex(vell, r);
  assert(r <= h - 1);
  drawSTofchildren(v, l - 1, h - r);
}



////////////////////////////////////////////////////////////
void heapify(double *a, int *indexarray, long i, long n) {
  long k;
  double temp, max;
  int tempprime;
  if (i * 2 > n)   /* i has one child */
    return;
  max = a[i * 2];
  k = i * 2;
  if (i * 2 + 1 <= n) {   /*i has two children*/
    if (a[i * 2 +1] > max) {
      max = a[i * 2 +1];
      k = i * 2 + 1;
    }
  }
  if (max <= a[i])   /*i has a child of value > than i's value*/
    {
      return;
    }
  temp = a[i]; a[i] = a[k]; a[k] = temp;
  tempprime = indexarray[i]; indexarray[i] = indexarray[k]; indexarray[k] = tempprime;
  heapify(a, indexarray, k, n);
}  /*heapify*/

////////////////////////////////////////////////////////////

void sort(double *a, int *indexarray, long n) {
  long j;
  double temp;
  int tempprime;
  for (j = n ; j >= 2; j--) {  /*interchange a[1] and a[j]*/
    temp = a[1]; a[1] = a[j]; a[j] = temp;
    tempprime = indexarray[1]; indexarray[1] = indexarray[j]; indexarray[j] = tempprime;
    heapify(a, indexarray, 1 , j-1);
  }    
}

////////////////////////////////////////////////////////////

void buildheap(double *a, int *indexarray, long n) {
  long j;
  for (j = n; j >= 1; j--) {
    heapify(a, indexarray, j, n);
  }
}  /*buildheap*/

////////////////////////////////////////////////////////////

void heapsort_howard(double *a, int *indexarray, long n) {
  buildheap(a, indexarray, n);
  sort(a, indexarray, n);
}






/*
  if (DEBUG) Rprintf("Line 128: numchildrenof[%d]=%d.\n",v,numchildrenof[v]);
  v1=(childstart[v]-1)+indexa[v][1]; // child of v of smallest size.
  if (DEBUG) Rprintf("Line 129: indexa[%d][1]=%d.\n",v,indexa[v][1]);
  vdv=(childstart[v]-1)+indexa[v][dv]; // child of v of largest size.
  if (h==2) {
  Rprintf("{%d}\n",v);
  if (dv==1) {
  if (DEBUG) Rprintf("Line 139: ");
  Rprintf("T_%d\n",v1);
  }
  else {
  Rprintf("Other=");
  for (i=1; i<=numchildrenof[v]; ++i) {
  if (DEBUG) Rprintf("Line 145: ");
  Rprintf("T_%d",(childstart[v]-1)+indexa[v][i]);
  if (i<numchildrenof[v]) Rprintf(" union ");
  }
  Rprintf("\n");
  }
  return;
  }
 
  Rprintf("{%d}; weight=%lf\n",v,weight[v]);
  r=rightcount[v][dv][h-1]; // "h-1" because we've already printed "{v}".
  if (DEBUG) Rprintf("Line 156: rightcount[v=%d][dv=%d][h-1=%d]=%d.\n",
  v,dv,h-1,r);
  if (DEBUG) Rprintf("Line 158: about to call drawSTrootedatonevertex(%d,%d).\n", vdv,r);
  drawSTrootedatonevertex(vdv,r);
  hprime=h-(1+r);
  assert(1<=hprime);
  if (DEBUG) Rprintf("Line 162: about to call drawSTofchildren(%d,%d,%d).\n",
  v,dv-1,hprime);
  drawSTofchildren(v,dv-1,hprime);
 
*/
