#include <R.h>
#include <Rmath.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <assert.h>
#include <time.h>
#define DEBUG 0
#define INFTY ((float)4.4444444444444444e30)
#define INTINFTY 1000000000
#define MAXN 500000
#define MAXK 200
#define MAXW 300000000
#define MAXNUMNEWPAIRS MAXW
#define ASSERT 1
#define VERBOSE 1
#define LOGBASE 2.0 // will become 2.0 later
#define logofLOGBASE (log(LOGBASE))

struct achildorders { // "a" stands for "active"
  int nac; // "nac" = "number of active children"
  int *child; // child[i] is defined for i=0,1,2,...,nac.
  int *orderofST;  // the order (i.e., number of parts) in the summary tree
  // rooted at "child".
  // orderofST[i] is defined for i=0,1,2,...,nac.
};
struct achildorders **newlist, **oldlist;
struct achildorders **bestlist;
int **bits;
double *alpha;

double partialsum;
int vi;
float *clusterweights;
float lnD;
FILE *treefile, *weightfile, *mainoutputfile, *outputtreefile;
FILE *outputclusterfile;
// FILE *roundedweightsfile;
long *sumoffirstsizes_v;
// sumoffirstsizes_v[l] is the sum, over the first l
// children of v, of the size of that child.  The node v is implicit.
int *sumoffirstnumdescendants_v;
// sumoffirstnumdescendants_v[l] is the sum, over the first l
// children of v, of the number of descendants of that child.
// Node v is implicit.
float **F;
// F[v][k] is the maximum entropy of a k-node summary tree of T_v.
// g(v,l,k,w)=max entropy of a k-node ST of T_v1 union T_v2 union ... union T_vl
// whose "other" cluster has weight w.
float **g_vandlminus1, **g_vandl;
// g_vandlminus1[k][w]=g[v][l-1][k][w] and g_vandl[k][w]=g[v][l][k][w].
float *f_vk;
// f_vk[w] is the same as f_v(k,w) in the writeup.
double lnK,lnn;
char *mainoutputfilename, *epsstring, *treefilename, *weightsfilename;
char *outputtreefilename, *outputclusterfilename;

int *weight;
double *origweight, *scaledweightsum, *scaledweight, *fracpart;
double *origsize;
long *sumofweights;
long *size;
double *scaledsize;
// descendants x of v;  origsize is the same thing for origweight[x].
int *numchildren, *childstart, *childend, *numdescendants, *UB;
int *childindex;
int n, K;
int numclusterssofar;

////////////////////////////////////////////////////////////////////////////////

int ifloor(double z) {
  int intfloor;
  
  intfloor=(int) floor(z);
  if (ASSERT) assert((intfloor <= z) && (intfloor + 1 > z));
  return(intfloor);
}


////////////////////////////////////////////////////////////////////////////////

void dotherounding(int v, int sign) {
  int i;
  
  if (childstart[v] == 0) { // v is a leaf
    weight[v] = ifloor(scaledweight[v]) + sign;
    //if (DEBUG) fprintf(mainoutputfile, "weight[%d]=%d.\n", v, weight[v]);
    return;
  }
  
  // Now we know that v is not a leaf.
  weight[v] = ifloor(scaledweight[v]) + sign;
  //if (DEBUG) fprintf(mainoutputfile, "weight[%d]=%d.\n", v, weight[v]);
  
  for (i = 1; i <= numchildren[v]; ++i) {
    vi = childstart[v] + (i - 1);
    dotherounding(vi, bits[v][i]);
  }
}

////////////////////////////////////////////////////////////////////////////////

void drawtreeandcomputeclusterweights(int k, int v) {
  int ll,len,sumoforders,node,order;
  double sumoforigsizesofactivenodes;
  double sizeofothernode,sumofallclustersizes,w;
  
  if (ASSERT) assert((1<=k)&&(k<=numdescendants[v]));
  sumofallclustersizes=0;
  
  if (k==1) {
    // Produce cluster T_v:
    ++numclusterssofar;
    w=clusterweights[numclusterssofar]=origsize[v];
    sumofallclustersizes+=w;
    Rprintf("Cluster T_%d of weight %lf.\n",v,w);
    return;
  }
  
  // Now we may assume that k>=2.
  
  // Produce cluster {v}:
  ++numclusterssofar;
  w=clusterweights[numclusterssofar]=origweight[v];
  sumofallclustersizes+=w;
  Rprintf("Cluster {%d} of weight %lf.\n",v,w);
  
  len=bestlist[v][k-1].nac;
  if (ASSERT) assert(len>=0);
  sumoforders=0;
  for (ll=1; ll<=len; ++ll) {
    sumoforders+=bestlist[v][k-1].orderofST[ll];
  }
  if (ASSERT) assert(sumoforders+(len<numchildren[v])==k-1);
  // Explanation:  There is an "other" cluster if and only if
  // len<numchildren[v].  If this is true, then the sum of the orders should
  // be k-2.  If not, then the sum of orders should be exactly k-1.
  
  sumoforigsizesofactivenodes=0;
  for (ll=1; ll<=len; ++ll) {
    node=bestlist[v][k-1].child[ll];
    order=bestlist[v][k-1].orderofST[ll];
    sumoforigsizesofactivenodes+=origsize[node];
    if (order==1) { // basis of induction
      // Produce cluster T_node:
      ++numclusterssofar;
      w=clusterweights[numclusterssofar]=origsize[node];
      sumofallclustersizes+=w;
      //fprintf(outputtreefile,"(%d-->T_%d) ",v,node);
      Rprintf("Cluster T_%d of weight %lf.\n",node,w);
    }
    else {
      //fprintf(outputtreefile,"(%d-->%d) ",v,node);
      drawtreeandcomputeclusterweights(order,node);
      // recursive step
    }
  }
  
  if (len<numchildren[v]) {
    // Produce "other" cluster:
    sizeofothernode=origsize[v]-origweight[v]-sumoforigsizesofactivenodes;
    //fprintf(outputtreefile,"(%d-->\"other_%d\") ",v,v);
    ++numclusterssofar;
    w=clusterweights[numclusterssofar]=sizeofothernode;
    sumofallclustersizes+=w;
    Rprintf("\"other\" child of %d of weight %lf.\n",v,w);
  }
  
  // Now make sure the sum of the cluster weights approx. matches the original sum:
  assert((fabs(sumofallclustersizes)-origsize[1])<=0.0001*origsize[1]);
}



/////////////////////////////////////////////////////////////////////////////////////////

double gg(int D, int k, long W) {
  double gval;
  
  
  //fprintf(mainoutputfile,"In gg: D,k,W=%d %d %ld.\n",D,k,W);
  if (ASSERT) assert(k>=1);
  if (ASSERT) assert(D>=1);
  if (ASSERT) assert(W>=1);
  gval=((2.0/log(2.0))*D)/W*(1.0+lnK+log(1.0*W)-lnD);
  return(gval);
}

// @@ inline
float xlgx(float x) {
  // assert(x>=0);
  if (x==0) return (0);
  else return (x*log(x)/logofLOGBASE);
}

// @@ inline
float comb(float h1, int w1, float h2, int w2) {
  float q; // Hofq;
  if ((h1<0)||(h2<0)) return(-INFTY);
  if ((w1==0)&&(w2==0)) return (0);
  if (w1==0) return(h2); if (w2==0) return (h1);
  q=((float) w1)/((float) w1+w2);
  return(q*h1+(1-q)*h2-(q*log(q)+(1.0-q)*log(1.0-q))/logofLOGBASE);
  // since q is neither 0 nor 1.
}

////////////////////////////////////////////////////////////////////////////////////////////

/*
 void append(int node, int order, int k, int w) {
 // Append node "child" and order "order" to newlist[k][1+w].
 int num;
 
 num=++(newlist[k][1+w].nac); // Increment first, and then set num to the result.
 if (ASSERT) assert(num>=1);
 newlist[k][1+w].child[num]=node;
 newlist[k][1+w].orderofST[num]=order;
 
 }
 */

////////////////////////////////////////////////////////////////////////////////////////////

// @@ inline
void copy(int oldk, int oldwprime, int newk, int newwprime) {
  // Copy oldlist[oldk][oldwprime] to newlist[newk][newwprime].
  
  int ll;
  
  newlist[newk][newwprime].nac=oldlist[oldk][oldwprime].nac;
  for (ll=1; ll<=oldlist[oldk][oldwprime].nac; ++ll) {
    newlist[newk][newwprime].child[ll]=oldlist[oldk][oldwprime].child[ll];
    newlist[newk][newwprime].orderofST[ll]=oldlist[oldk][oldwprime].orderofST[ll];
  }
}

////////////////////////////////////////////////////////////////////////////////////////////

// @@ inline
void mallocandcopytobestlist(int k, int w, int v) {
  // Copy list oldlist[k][1+w] to list bestlist[v][k].
  
  int ll,len;
  if (ASSERT) assert((1<=k)&&(k<=K));
  if (ASSERT) assert(-1<=w);
  if (ASSERT) assert((1<=v)&&(v<=n));
  len=bestlist[v][k].nac=oldlist[k][1+w].nac;
  bestlist[v][k].child=(int *) malloc ((1+len)*sizeof(int));
  bestlist[v][k].orderofST=(int *) malloc ((1+len)*sizeof(int));
  assert(bestlist[v][k].orderofST!=NULL);
  
  for (ll=1; ll<=oldlist[k][1+w].nac; ++ll) {
    bestlist[v][k].child[ll]=oldlist[k][1+w].child[ll];
    bestlist[v][k].orderofST[ll]=oldlist[k][1+w].orderofST[ll];
  }
}
// @@@@@@@@@@@@@@ free space for bestlist!

////////////////////////////////////////////////////////////////////////////////////////////

//int main(int argc, char *argv[]) {
void Roptimal(int *R_K, int *R_n, int *R_numparents,
         double *R_epsilon, double *R_weight,
         int *R_childindex, int *R_childstart, int *R_childend) {
  
  time_t now;
  double maxabsdiff, diff;
  int sumnumchildren, location;
  int D; // upper bound on max absdiscrep
  int oldk, oldwprime, newk, newwprime;
  int ndvl, UBminusk1, bestw, maxkwithfinitecasevar;
  double bestentropy, entropy, err;
  int countofsteps,length, tempub1, tempub2;
  double maxvalreal;
  long ubsize,ubnode,ll;
  float arg1,arg3;
  int sss,oneplusw,lminus1;
  int indexoflargest;
  double largestval;
  double scalefactor,error;
  int exact;
  double weightsum,newweightsum;
  float val;
  int wstarval;
  int wprime;
  float Hprime;
  float *maxval_w;
  int *casevar;
  int **kstar;
  double maxnum;
  float sumofsizes, sumofminvals;
  //int j;
  int ii,degree;
  int svl;
  float H;
  int wL,wR,W;
  int l,vl,k1,k2;
  float combcost;
  int w,d,v,vprime,i,v1;
  int *parent,*depth;
  int maxdepth;
  int *minnodeofdepth,*maxnodeofdepth;
  int maxnodeseen;
  int seed;
  int k;
  //int len;
  int numparents;
  //double wt;
  long W0,Wopt;
  long mid;
  long left,right;
  double g1,g2,gval;
  // char treefilename[101],weightsfilename[101];
  double eps;
  
  countofsteps=0;
  // Now allocate space for formerly-statically-allocated arrays.
  
  
  treefilename= (char *) malloc (101*sizeof(char));
  epsstring= (char *) malloc (101*sizeof(char));
  mainoutputfilename= (char *) malloc (101*sizeof(char));
  weightsfilename= (char *) malloc (101*sizeof(char));
  outputtreefilename= (char *) malloc (101*sizeof(char));
  outputclusterfilename= (char *) malloc (101*sizeof(char));
  // printf("argv[1]=%s.\n",argv[1]);
  //len=strlen(argv[1]);
  //assert(len<=100);
  
  //strcpy(treefilename,argv[1]);
  //strcpy(weightsfilename,argv[1]);
  //strcpy(outputtreefilename,argv[1]);
  //strcpy(outputclusterfilename,argv[1]);
  //strcpy(mainoutputfilename,argv[1]);
  
  strcat(treefilename,"tree.txt");
  strcat(weightsfilename,"weights.txt");
  
  //treefile=fopen(treefilename,"r");
  //weightfile=fopen(weightsfilename,"r");
  // roundedweightsfile=fopen("attbigroundedweights.txt","w");
  
  // Some options:
  // mathgen
  // test2
  // attsample
  // yahoo
  
  // We will process the nodes in decreasing order
  // by depth, processing all nodes at depth d prior
  // to any at depth d-1.
  
  setvbuf( stdout, 0, _IONBF, 0); setvbuf( stderr, 0, _IONBF, 0);
  
  
  // Nodes will be labeled in increasing order by depth, with the root given label 1.
  // The children of v, if any, will be childstart[v], childstart[v]+1,..., childend[v].
  // If v has no children, then childstart[v]=childend[v]=0.
  
  if (DEBUG) printf("-INFTY=%lf.\n",-INFTY);
  
  //fscanf(treefile,"%d%d",&numparents,&n);
  //assert(n<=MAXN);

  n = R_n[0];
  numparents = R_numparents[0];

  lnn = log(1.0*n);
  
  
  // seed = time(0); srand48(seed);
  seed = 0; srand(seed);
  
  //printf("Don't forget to change stacksize via 'limit stacksize 100000'\n");
  //printf("Enter K=maximum value of k: ");
  //scanf("%d",&K);
  //assert(2<=K);
  //if (K>n) {
    //printf("Decreasing K to %d=n.\n",n);
    //K=n;
  //}
  
  K = R_K[0];

  lnK = log(1.0*K);

  //printf("Enter nonnegative epsilon (epsilon=0 being allowed only for integral weights): ");
  //scanf("%lf",&eps);
  
  eps = R_epsilon[0];

  assert(0 <= eps);
  assert(eps <= log(K)); // since max entropy of all instances
  // over k=1,2,...,K is ln(K), so there's no need for an eps>ln(K).
  // @@@ Binary or natural log?
  exact = (eps == 0);
  //sprintf(epsstring,"%s%0.5f","eps",eps);
  
  strcat(outputtreefilename, epsstring);
  strcat(outputtreefilename, "treesout.txt");
  strcat(outputclusterfilename, epsstring);
  strcat(outputclusterfilename, "clustersout.txt");
  strcat(mainoutputfilename, epsstring);
  strcat(mainoutputfilename, "mainoutput.txt");
  //mainoutputfile=fopen(mainoutputfilename,"w");
  
  //fprintf(mainoutputfile,"Using tree file '%s' and weight file '%s'.\n",treefilename,weightsfilename);
  //fprintf(mainoutputfile,"Using mainoutputfilename '%s'.\n",mainoutputfilename);
  //fprintf(mainoutputfile,"Using outputtreefilename '%s'.\n",outputtreefilename);
  //fprintf(mainoutputfile,"Using outputclusterfilename '%s'.\n",outputclusterfilename);
  //fprintf(mainoutputfile,"K=%d.\n",K);
  //fprintf(mainoutputfile,"eps=%lf.\n",eps);
  //fprintf(mainoutputfile,"numparents=%d and n=%d.\n",numparents,n);
  
  Rprintf("n=%d.\n",n);
  origweight=(double *)malloc ((1+n)*sizeof(double)); assert(origweight!=NULL);
  scaledweightsum=(double *)malloc ((1+n)*sizeof(double)); assert(scaledweightsum!=NULL);
  scaledweight=(double *)malloc ((1+n)*sizeof(double)); assert(scaledweight!=NULL);
  fracpart=(double *)malloc ((1+n)*sizeof(double)); assert(fracpart!=NULL);
  scaledsize=(double *)malloc ((1+n)*sizeof(double)); assert(scaledsize!=NULL);
  sumofweights=(long *) malloc((1+n)*sizeof(long)); assert(sumofweights!=NULL);
  size=(long *) malloc((1+n)*sizeof(long)); assert(size!=NULL);
  numchildren=(int *) malloc((1+n)*sizeof(int)); assert(numchildren!=NULL);
  childstart=(int *) malloc((1+n)*sizeof(int)); assert(childstart!=NULL);
  childend=(int *) malloc((1+n)*sizeof(int)); assert(childend!=NULL);
  childindex=(int *) malloc((1+numparents)*sizeof(int));
  numdescendants=(int *) malloc((1+n)*sizeof(int)); assert(numdescendants!=NULL);
  UB=(int *) malloc((1+n)*sizeof(int)); assert(UB!=NULL);
  parent=(int *) malloc((1+n)*sizeof(int)); assert(parent!=NULL);
  depth=(int *) malloc((1+n)*sizeof(int)); assert(depth!=NULL);
  minnodeofdepth=(int *) malloc((1+n)*sizeof(int)); assert(minnodeofdepth!=NULL);
  maxnodeofdepth=(int *) malloc((1+n)*sizeof(int)); assert(maxnodeofdepth!=NULL);
  origsize=(double *) malloc ((1+n)*sizeof(double)); assert(origsize!=NULL);
  alpha=(double *) malloc ((1+n)*sizeof(double)); assert(alpha!=NULL);
  weight=(int *) malloc ((1+n)*sizeof(int)); assert(weight!=NULL);

  Rprintf("after allocations.\n");

  
  // Declare the space for storing the bestlist lists:
  bestlist=(struct achildorders **) malloc ((1+n)*sizeof(struct achildorders *));
  assert(bestlist!=NULL);
  for (v=1; v<=n; ++v) {
    bestlist[v]=(struct achildorders *) malloc ((1+K)*sizeof(struct achildorders));
    assert(bestlist[v]!=NULL);
    // @@@ Don't forget to free bestlist!
  }
  // Now malloc the child and orderofST fields.
  
  // Do binary search to find the least W>=D/k.   Since
  // there are many different k's in 1..K, instead of doing it
  // separately, just note that gg(D/2)=4[1+ln k - ln 2]>ln k,
  // so we can start from D/2 instead of D/k.
  
  // This part is unused if exact is true.
  Wopt = -INTINFTY; // to prevent compiler warning
  // WARNING:  Security risk!  What if the user uses "name; rm *"
  // as the filename?  This will delete everything!
  now = time(NULL);
  //fprintf(mainoutputfile,"The time is now %s",ctime(&now));
  if (!exact) {
    // fprintf(mainoutputfile,"Enter upper bound D on maximum absdiscrep: ");
    // scanf("%d",&D);
    // fprintf(mainoutputfile,"D=%d.\n",D);
    D = 3*K; // because D=K+2KM, according to the writeup, and M=1.
    assert(D >= 1);
    lnD = log(1.0*D);
    left = (long) D/2; if (2*left < D) ++left; // left=ceiling(D/2).
    //if (DEBUG) fprintf(mainoutputfile,"line 465: left=%ld.\n",left);
    g1 = gg(D, K, left);
    if (ASSERT) assert(g1 > eps);
    maxnum = K;
    if (1.0/eps > maxnum) maxnum = 1.0/eps;
    if (10/log(2.0) > maxnum) maxnum = 10/log(2.0);
    right = W0 = (long)ceil((10/log(2.0))*D*log(maxnum)/eps);
    // fprintf(mainoutputfile,"line 472: right=%ld.\n",right);
    g2 = gg(D, K, right);
    if (ASSERT) assert(g2 <= eps);
    // Now do binary search to find least W in the interval
    // [left,right] such that gg(D,K,W)<=eps.
    // Maintain invariant that gg(D,K,left)>eps and gg(D,K,right)<=eps.
    // For fixed D,K, gg is decreasing for W>=D/k.
    while (right >= left + 2) {
      mid = (long)(left + right)/2;
      if (ASSERT) assert((left < mid) && (mid < right));
      //if (DEBUG) fprintf(mainoutputfile,"line 482: mid =%ld.\n",mid);
      gval = gg(D, K, mid);
      //if (DEBUG) fprintf(mainoutputfile,"gg(%ld)=%lf.\n",mid,gval);
      if (gval > eps) {
        left = mid;
        //if (DEBUG) fprintf(mainoutputfile,"Line 488: Just set left to %ld.\n",mid);
      }
      else {
        right = mid;
        //if (DEBUG) fprintf(mainoutputfile,"Line 492: Just set right to %ld.\n",mid);
      }
      //if (DEBUG) fprintf(mainoutputfile,"Now [%ld,%ld].\n",left,right);
    }
    if (ASSERT) assert(right == left + 1);
    if (ASSERT) assert((gg(D, K, left) > eps) && (gg(D, K, right) <= eps));
    Wopt = right;
    //fprintf(mainoutputfile,"Wopt=%ld.\n",Wopt);
    // Later we will scale the input weights so that they
    // add up exactly to Wopt, if exact=0.
  }
  
  F = (float **) malloc ((1 + n)*sizeof(float *));
  assert(F != NULL);
  for (i = 0; i <= n; ++i) {
    F[i] = (float *) malloc((1 + K)*sizeof(float));
    assert(F[i] != NULL);
  }
  
  for (v=0; v<=n; ++v) { // node 0 doesn't really exist, though.
    childstart[v]=childend[v]=0;
    numchildren[v]=0;
  }
  for (i=0; i <= numparents; ++i) {
    childindex[i] = 0;
  }
  
  maxnodeseen=-1;
  sumnumchildren=0;
  for (ii=1; ii<=numparents; ++ii) {
    //fscanf(treefile,"%d%d%d",&i,&j,&k);
    //if (ASSERT) assert((1<=i)&&(i<=MAXN));
    childindex[ii] = R_childindex[ii - 1];
    childstart[childindex[ii]] = R_childstart[ii - 1];
    childend[childindex[ii]] = R_childend[ii - 1];
    //if (j>0) numchildren[i]=k-j+1;
    if (R_childstart[ii - 1] > 0) {
      numchildren[childindex[ii]] = R_childend[ii - 1] - R_childstart[ii - 1] + 1;
    }
    sumnumchildren += numchildren[childindex[ii]];
    //if ((ii<=10)||(ii==numparents)) fprintf(mainoutputfile,"childstart[%d]=%d and childend[%d]=%d.\n",i,childstart[i],i,childend[i]);
    //if (i!=0) if (ASSERT) assert((j!=0)&&(i<j)&&(j<=k));
    if (R_childend[ii - 1] > maxnodeseen) maxnodeseen = R_childend[ii - 1];
  }
  //fprintf(mainoutputfile,"sumnumchildren=%d.\n",sumnumchildren);
  
  if (ASSERT) assert(n==maxnodeseen);
  //fprintf(mainoutputfile,"n=%d.\n",n);
  if (ASSERT) assert(1<=n);
  if (ASSERT) assert(n<=MAXN);
  
  weightsum=0;
  indexoflargest=0; largestval=-INFTY;
  for (ii=1; ii<=n; ++ii) {
    //fscanf(weightfile, "%lf", &wt);
    //origweight[ii]  = wt;
    origweight[ii]  = R_weight[ii - 1];
    if (R_weight[ii - 1] > largestval) {
      largestval = R_weight[ii - 1];
      indexoflargest = ii;
    }
    weightsum += R_weight[ii - 1];
    //if (ASSERT) assert((0<=origweight[ii])&&(origweight[ii]<=MAXW));
    //if (ii<=10)fprintf(mainoutputfile,"origweight[%d]=%lf.\n",ii,origweight[ii]);
    //if (ii==n) fprintf(mainoutputfile,"\norigweight[%d]=%lf.\n\n",ii,origweight[ii]);
  }
  //if (DEBUG) fprintf(mainoutputfile,"weightsum=%lf.\n",weightsum);
  //if (ASSERT) assert(weightsum>0);
  
  // Now scale the input weights so that they add exactly to Wopt.
  if (!exact) {
    scalefactor=Wopt/weightsum;
    //fprintf(mainoutputfile,"scalefactor=%lf.\n",scalefactor);
  }
  else {
    scalefactor=-INFTY; // and unused
  }
  if (!exact) {
    newweightsum=0;
    scaledweightsum[0]=0;
    //if (DEBUG) fprintf(mainoutputfile,"scaledweightsum[n]=%lf.\n",scaledweightsum[n]);
    for (ii=1; ii<=n; ++ii) {
      scaledweight[ii]=scalefactor*origweight[ii];
      scaledweightsum[ii]=scaledweightsum[ii-1]+scaledweight[ii];
    }
    error=Wopt-scaledweightsum[n];
    //fprintf(mainoutputfile,"error in summation of rounded weights is %lf.\n",error);
    // This should be 0, but, due to round-off error,
    // it might not be.  Add (error+1.0e-6) onto
    // the largest scaledweight, and recompute the sums.
    if (ASSERT) assert(fabs(error)<0.01);
    scaledweight[indexoflargest]+=(error+1.0e-6);
    scaledweightsum[0]=0;
    for (ii=1; ii<=n; ++ii) {
      scaledweightsum[ii]=scaledweightsum[ii-1]+scaledweight[ii];
    }
    if (DEBUG) {
      for (ii=1; ii<=n; ++ii) {
        //fprintf(mainoutputfile,"%lf\n",scaledweight[ii]);
      }
    }
    
    /*
     Now we have to round the scaledweights.  We leave the integral
     part of scaledweight[v] untouched, so let
     fracpart[v]=scaledweight[v]-floor(scaledweight[v]).
     
     Let D_v be the set of descendants of v.
     Now we store a value alpha[v] in (0,1] for all v.
     This is the discrepancy of D_v in the so-called "positive"
     rounding of D_v in which
     every u in D_v has discrepancy in (-1,1].
     There is also a second "negative" rounding of D_v in which disc(D_v) in
     (-1,0] and every u in D_v has discrepancy in (-1,1].
     We also store bits[v,sign], where sign is in {0,1} (sign=1 meaning
     discrep of D_v is in (0,1], and sign=0 meaning discrep. of D_v is in
     (-1,0])), a vector of length
     1+numchildren[v].  The first bit tells the first choice, which
     corresponds to v itself.  It turns out that this first bit
     always equals "sign", and therefore we don't bother to compute
     it.  The later bits are determined as follows.
     
     Let the children of v be, in some order, v_1,v_2,v_3,...,v_d.
     
     If sign=1, then we do the following.
     
     Let partialsum=(1-fracpart[v]).  Set bits[v,1]_0=1=sign.
     For i=1,2,...,d, do:
     If partialsum+alpha_{v_i}<=1, set bits[v,1]_i=1 and
     partialsum+=alpha_{v_i}.
     Else set bits[v,1]_i=0 and partialsum+=(alpha_{v_i}-1).
     
     If sign=0, then we do the following.
     
     Let partialsum=(-fracpart[v]).  Set bits[v,0]_0=0=sign.
     For i=1,2,...,d, do:
     If partialsum+(alpha_{v_i}-1)>-1, set bits[v,0]_i=0 and
     partialsum+=(alpha_{v_i}-1).
     Else set bits[v,0]_i=1 and partialsum+=alpha_{v_i}.
     
     However, it turns out that after b_0, the bits are precisely the
     same!  (This is easy to prove.)
     Therefore we don't bother with the sign=0 case.
     Furthermore, since bits[v,sign]_0=sign, we omit the 0th bit as well.
     
     In other words, we just do the following:
     Let partialsum=(1-fracpart[v]).
     For i=1,2,...,d, do:
     If partialsum+alpha_{v_i}<=1, set bits[v,1]_i=1 and
     partialsum+=alpha_{v_i}.
     Else set bits[v,1]_i=0 and partialsum+=(alpha_{v_i}-1).
     
     Later we use the bits vectors to round the weights.  I will show
     how later.
     */
    bits=(int **) malloc((1+n)*sizeof(int *));
    assert(bits!=NULL);
    
    for (v=1; v<=n; ++v) {
      fracpart[v]=scaledweight[v]-ifloor(scaledweight[v]);
      bits[v]=(int *) malloc((1+numchildren[v])*sizeof(int));
      assert(bits[v]!=NULL);
    }
    
    for (v=n; v>=1; --v) {  // in this order so that a node is
      // processed after its children are.
      if (childstart[v]==0) { // a leaf
        partialsum=1-fracpart[v];
        alpha[v]=partialsum;
        //if (DEBUG) fprintf(mainoutputfile,"alpha[%d]=%lf.\n",v,alpha[v]);
        if (ASSERT) assert((0<alpha[v])&&(alpha[v]<=1));
        // No need to define bits[v] for leaves.
      }
      else {
        partialsum=1-fracpart[v];
        for (i=1; i<=numchildren[v]; ++i) {
          vi=childstart[v]+i-1;
          bits[v][i]=(partialsum+alpha[vi]<=1);
          //if (DEBUG) fprintf(mainoutputfile,"bits[%d][%d]=%d.\n",v,i,bits[v][i]);
          partialsum+=(alpha[vi]-1)+bits[v][i];
          // if bits[v][i]=1, we add alpha[vi],
          // which is the discrep. in the positive case;
          // if bits[v][i]=0, we add alpha[vi]-1,
          // which is the discrepancy in the negative case.
          if (ASSERT) assert((0<partialsum)&&(partialsum<=1));
        }
        alpha[v]=partialsum;
        //if (DEBUG) fprintf(mainoutputfile,"alpha[%d]=%lf.\n",v,alpha[v]);
      }
    }
    // Don't forget to add the floor(scaledweight[v]) back in!
    
    // Now actually round the fracpart[v]'s.
    
    dotherounding(1,0); // This means, "Produce the rounding
    // of the descendants of node 1 (the root) in which D_1 has
    // negative discrepancy, i.e., in (-1,0]."  Hence, if the sum of
    // the scaled weights is integral, then, since the absdiscrep is
    // less than 1, the sum of the new weights must be the same.
    
    /*
     for (v=1; v<=n; ++v) {
     fprintf(roundedweightsfile,"%d\n",weight[v]);
     }
     exit(0);
     */
    
    now=time(NULL);
    //fprintf(mainoutputfile,"The time is now %s",ctime(&now));
  }
  
  if (exact) {
    //fprintf(mainoutputfile,"\nWarning:  All weights must be integral since exact solution is sought!\n\n");
    for (v=1; v<=n; ++v) {
      weight[v]=(int) origweight[v];
      assert(weight[v]==origweight[v]);
      scaledweight[v]=weight[v]; // Just defined to prevent a bug if exact=1.
    }
  }
  
  // Test the input:
  
  for (v=0; v<=n; ++v) {
    // fprintf(mainoutputfile,"childstart[%d],childend[%d]=%d %d.\n",v,v,
    //   childstart[v],childend[v]);
    if (v>=1) if (ASSERT) assert(weight[v]>=0);
    if (ASSERT) assert(childstart[v]<=childend[v]);
    if (ASSERT) assert(0<=childstart[v]);
    if (ASSERT) assert(childend[v]<=n);
    if (childstart[v]==0) if (ASSERT) assert(childend[v]==0);
    if (childend[v]==0) if (ASSERT) assert(childstart[v]==0);
    if (childstart[v]!=0) if (ASSERT) assert(childstart[v]>v);
    numdescendants[v]=1; // initialization
  }
  
  // Let's first compute the size of each node.
  
  maxabsdiff=-INFTY;
  location=-INTINFTY;
  for (v=n; v>=1; --v) {
    if (childstart[v]==0) {
      size[v]=weight[v];
      origsize[v]=origweight[v];
      scaledsize[v]=scaledweight[v];
    }
    else {
      scaledsize[v]=scaledweight[v];
      size[v]=weight[v]; origsize[v]=origweight[v]; // This is initialization only.
      for (vprime=childstart[v]; vprime<=childend[v]; ++vprime) {
        size[v]+=size[vprime]; origsize[v]+=origsize[vprime];
        scaledsize[v]+=scaledsize[vprime];
        numdescendants[v]+=numdescendants[vprime];
      }
      diff=fabs(scaledsize[v]-size[v]);
      if (diff>maxabsdiff) {
        maxabsdiff=diff;
        location=v;
      }
      //if (DEBUG) fprintf(mainoutputfile,"numchildren[%d]=%d and numdescendants[%d]=%d.\n",v,numchildren[v],v,numdescendants[v]);
    }
    
    UB[v]=K; if (numdescendants[v]<UB[v]) UB[v]=numdescendants[v];
    //if (DEBUG) fprintf(mainoutputfile,"numchildren[%d]=%d and numdescendants[%d]=%d and UB[%d]=%d.\n", v,numchildren[v],v,numdescendants[v],v,UB[v]);
    // UB[v]=min{K,numdescendants[v]}.  There is no reason to
    // compute the entropy of a k-node summary tree ("ST")
    // rooted at v if v has fewer than k descendants, since it does
    // not exist.
  }
  
  //fprintf(mainoutputfile,"size[1]=%ld.\n",size[1]);
  if (!exact) {
    //fprintf(mainoutputfile,"maxabsdiff=%lf.\n",maxabsdiff);
    //fprintf(mainoutputfile,"location=%d.\n",location);
    assert(maxabsdiff<=1);
    //fprintf(mainoutputfile,"scaledsize[1]=%lf.\n",scaledsize[1]);
    assert(maxabsdiff<=1);
    assert(fabs(size[1]-scaledsize[1])<0.001);
  }
  
  sumofsizes=sumofminvals=0;
  
  // The nodes are labeled in increasing order by depth.  Therefore,
  // we will process them in decreasing order by depth, skipping the leaf nodes.
  
  // Let F_v[k] be max_{w=-1}^{size[v]} of f_v[k,w],
  // the maximum entropy of any k-node summary tree for T_v.
  
  // Compute parent pointers:
  parent[1]=0; // root has no parent.
  for (v=1; v<=n; ++v) {
    if (childstart[v]!=0) {
      for (i=childstart[v]; i<=childend[v]; ++i) {
        parent[i]=v;
        if (ASSERT) assert(parent[i]<i);
      }
    }
  }
  
  // Now compute nodes' depths.
  for (v=1; v<=n; ++v) {
    if (v==1) depth[v]=0;
    else {
      d=depth[v]=1+depth[parent[v]];
      if (d>depth[v-1]) {
        minnodeofdepth[d]=v;
        maxnodeofdepth[d-1]=v-1;
      }
    }
    // fprintf(mainoutputfile,"depth[%d]=%d.\n",v,depth[v]);
  }
  maxdepth=depth[n];
  //fprintf(mainoutputfile,"maxdepth=%d.\n",maxdepth);
  minnodeofdepth[0]=1; maxnodeofdepth[0]=1;
  maxnodeofdepth[maxdepth]=n;
  for (d=0; d<=maxdepth; ++d) {
    //fprintf(mainoutputfile,"minnodeofdepth[%d]=%d; maxnodeofdepth[%d]=%d.\n",
            //d,minnodeofdepth[d],d,maxnodeofdepth[d]);
  }
  
  //////////////////////////////////////////////////////////////////
  
  casevar=(int *) malloc ((1+K)*sizeof(int));
  assert(casevar!=NULL);
  
  maxval_w=(float *) malloc ((1+K)*sizeof(float));
  assert(maxval_w!=NULL);
  
  kstar=(int **) malloc ((1+K)*sizeof(int *));
  assert(kstar!=NULL);
  //fprintf(mainoutputfile,"sizeof(int)=%d.\n",(int) sizeof(int));
  for (k=0; k<=K; ++k) {
    kstar[k]=(int *) malloc ((2+size[1])*sizeof(int));
    // As node 1 is the root, I am making kstar[k] big enough to work for all nodes.
    assert(kstar[k]!=NULL);
  }
  
  f_vk=(float *) malloc ((2+size[1])*sizeof(float)); // mallocked just once, and made
  // large enough for entire program.
  assert(f_vk!=NULL);
  
  // Now process one level of nodes at a time, in decreasing
  // order by level.  If a node is a leaf, we define the variables directly.
  // If not, we combine the values on its children to get the value for the node.
  
  for (v=1; v<=n; ++v) {
    for (k=1; k<=K; ++k) {
      F[v][k]=-INFTY;
    }}
  
  // Now we do all levels.
  
  for (d=maxdepth; d>=0; --d) {
    // Process level d.
    
    for (v=maxnodeofdepth[d]; v>=minnodeofdepth[d]; --v) {
      //fprintf(mainoutputfile,"Now processing v=%d; size[%d]=%ld.",v,v,size[v]);
      F[v][1]=0; // @@@ Don't I have to save more here, in order to draw the trees?
      // There is a 1-node summary tree, of entropy 0, rooted at v, for all v.
      
      if (childstart[v]==0) { // i.e., v is a leaf
        //fprintf(mainoutputfile," This node is a leaf.\n"); // and do nothing else.
      }
      
      else { // Now process a nonleaf.
        //fprintf(mainoutputfile," %d is a nonleaf.\n",v);
        sumoffirstsizes_v=(long *) malloc ((1+numchildren[v])*sizeof(long));
        sumoffirstnumdescendants_v=(int *) malloc ((1+numchildren[v])*sizeof(int));
        sumoffirstsizes_v[0]=0;
        sumoffirstnumdescendants_v[0]=0;
        for (l=1; l<=numchildren[v]; ++l) {
          vl=childstart[v]+l-1; svl=size[vl];
          sumoffirstsizes_v[l]=sumoffirstsizes_v[l-1]+svl;
          sumoffirstnumdescendants_v[l]=sumoffirstnumdescendants_v[l-1]+numdescendants[vl];
        }
        
        if (VERBOSE) //fprintf(mainoutputfile,"v=%d has %d children and %d descendants.\n", v,numchildren[v],numdescendants[v]);
        
        
        // v=341729 is troublesome for attbig.
        
        if (ASSERT) assert(childstart[v]!=0); // i.e., v is not a leaf.
        v1=childstart[v];
        
        // WARNING:  Since indices in C start at 0, gv[l][k][1+w] will refer to w.
        g_vandlminus1=(float **) malloc ((1+K)*sizeof(float *));
        // Initially, g_vandlminus1[k][1+w] refers to l=1, which refers to the first child of v.
        assert(g_vandlminus1!=NULL);
        for (k=1; k<=UB[v]; ++k) {
          g_vandlminus1[k]=(float *) malloc ((2+sumoffirstsizes_v[1])*sizeof(float));
          assert(g_vandlminus1[k]!=NULL);
        } // for (k
        for (k=1; k<=UB[v]; ++k) {
          for (wprime=0; wprime<=1+sumoffirstsizes_v[1]; ++wprime) {
            g_vandlminus1[k][wprime]=-INFTY;
          }}
        
        g_vandlminus1[1][1+(-1)]=0;  // =gv[1][1][0]
        if (size[childstart[v]]>sumoffirstsizes_v[1]) {
          //fprintf(mainoutputfile,"About to fail assertion on line 855 because v=%d, childstart[%d]=%d, size[childstart[%d]]=%ld yet sumoffirstsizes_v[1]=%ld.\n", v,v,childstart[v],v,size[childstart[v]],sumoffirstsizes_v[1]);
        }
        if (ASSERT) assert(size[v1]==sumoffirstsizes_v[1]);
        g_vandlminus1[1][1+size[v1]]=0; // There is a 1-node 0-entropy summary tree rooted at v1 with "other"
        // of weight size[v1].
        for (k=1; k<=UB[v1]; ++k) {
          g_vandlminus1[k][1+(-1)]=F[v1][k]; // initializing for first child
          val=g_vandlminus1[k][0];
          if (val>=0) {
            // Note that k might be numdescendants[v] here and might
            // therefore exceed numdescendants[childstart[v]], but
            // we are initializing F[v][k] for k up to K, not UB[v].
            if (k>numdescendants[v1]) {
              //fprintf(mainoutputfile,"About to fail assertion for v=%d.  val=%lf, k=%d,\n",v,val,k);
              //fprintf(mainoutputfile,"yet numdescendants[v1]=%d.\n", numdescendants[v1]);
            }
            if (ASSERT) assert(k<=numdescendants[v1]);
          }
        }
        
        /////////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Now declare space for and initialize the arrays keeping track of the active children.
        
        oldlist=(struct achildorders **) malloc ((1+K)*sizeof(struct achildorders *));
        // fprintf(mainoutputfile,"Just mallocked oldlist on line 624 for v=%d and outside of l and w loops.\n",v);
        assert(oldlist!=NULL);
        for (k=1; k<=UB[v]; ++k) {
          oldlist[k]=(struct achildorders *) malloc ((2+sumoffirstsizes_v[1])*sizeof(struct achildorders));
          assert(oldlist[k]!=NULL);
        }
        // Now initialize oldlist:
        for (k=1; k<=UB[v]; ++k) {
          for (w=-1; w<=sumoffirstsizes_v[1]; ++w) { // because oldlist refers only to child 1 initially
            oldlist[k][1+w].nac=-INTINFTY; // -INTINFTY is a flag meaning that there's no such tree.
            oldlist[k][1+w].child=(int *) malloc ((1+k)*sizeof(int)); // Can't have more than k active nodes in a k-node summary tree.
            oldlist[k][1+w].orderofST=(int *) malloc ((1+k)*sizeof(int)); // Can't have more than k active nodes in a k-node summary tree.
          }}
        
        // Now initialize arrays for l=1.
        
        // There is a (k=1)-node summary tree of T_v1 with an "other" cluster of weight size[v1];
        // it has no active nodes at all.  The fact that oldlist[1][1+size[v1]].nac is 0,
        // and not -INTINFTY, reflects the fact that there *is* a 1-node summary tree rooted at v1,
        // with an "other" cluster, but with zero active nodes.
        
        oldlist[1][1+size[v1]].nac=0;
        
        // We do not change oldlist[][].child or oldlist[][].orderofST because there are
        // no active children.
        
        // For each k such that F[v1][k]>=0, there is a k-node summary tree rooted at v1
        // with no "other" cluster.  This is distinct from the previous case of a 1-node summary
        // tree, even if k=1, with no "other" cluster.
        for (k=1; k<=UB[v1]; ++k) {
          if (F[v1][k]>=0) {
            oldlist[k][1+(-1)].nac=1;
            oldlist[k][1+(-1)].child[1]=v1; // Make v1 the first node on the list
            oldlist[k][1+(-1)].orderofST[1]=k; // and set its order to be k.
          }
        }
        /////////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        // Now we do Lemma 3 of the writeup.
        for (l=2; l<=numchildren[v]; ++l) {
          //if (l%100==0) fprintf(mainoutputfile,"Now processing child l=%d of %d of node %d.\n",l,numchildren[v],v);
          
          ubsize=sumoffirstsizes_v[l];
          lminus1=l-1;
          vl=childstart[v]+lminus1; svl=size[vl];
          ndvl=numdescendants[vl];
          
          ////////////////////////////
          
          // Now we have to define g_vandl[k][1+w]=g[v][l][k][1+w], as in the writeup (v,l
          // being implicit), as the maximum
          // entropy of a k-node summary tree for T_{v_1} union ... union T_{v_l}
          // (note that v is omitted), such that the sum of the sizes of the "other"
          // v_i's is w.   We use w=-1 to mean "there is no 'other' node."
          //////////////////////////////////////////////////////////////////////////////////////////////
          
          // First we malloc the space, which we will have to free later.
          
          g_vandl=(float **) malloc ((1+K)*sizeof(float *));
          assert(g_vandl!=NULL);
          for (k=1; k<=UB[v]; ++k) {
            g_vandl[k]=(float *) malloc ((2+sumoffirstsizes_v[l])*sizeof(float));
            // fprintf(mainoutputfile,"Just mallocked %d floats for g_vandl[%d].\n",2+sumoffirstsizes_v[l],k);
            assert(g_vandl[k]!=NULL);
          } // for (k
          
          for (k=1; k<=UB[v]; ++k) {
            for (wprime=0; wprime<=1+ubsize; ++wprime) {
              g_vandl[k][wprime]=-INFTY;
            }}
          
          //////////////////////////////////////////////////////////////////////////////////////////////
          
          // Oldlist implicitly refers to child l-1, not l, of node v.  Newlist will implicitly refer to child l of node v.
          newlist=(struct achildorders **) malloc ((1+K)*sizeof(struct achildorders *)); // newlist refers to the current value of l.
          assert(newlist!=NULL);
          for (k=1; k<=UB[v]; ++k) {
            newlist[k]=(struct achildorders *) malloc ((2+sumoffirstsizes_v[l])*sizeof(struct achildorders));
            // @@ WAS: newlist[k]=(struct achildorders *) malloc ((2+size[1])*sizeof(struct achildorders));
            assert(newlist[k]!=NULL);
          }
          for (k=1; k<=UB[v]; ++k) {
            for (w=-1; w<=ubsize; ++w) {
              newlist[k][1+w].nac=-INTINFTY; // -INTINFTY is a flag meaning that there's no such tree.
              newlist[k][1+w].child=(int *) malloc ((1+k)*sizeof(int)); // Can't have more than k active nodes in a k-node summary tree.
              newlist[k][1+w].orderofST=(int *) malloc ((1+k)*sizeof(int)); // Can't have more than k active nodes in a k-node summary tree.
            }}
          
          
          ///////////////////////////////////////////////////////////////////////////////////////////////
          
          sss=sumoffirstsizes_v[lminus1];
          
          ///////////////////////////////////////////////////////////////////////////////////////////////
          
          // Now we have to compute the values of line 2 and the three quantities (a), (b), (c) which appear
          // in line 3 of Lemma 3 in the writeup.
          
          ubnode=UB[v];
          // Try: ubnode=sumoffirstnumdescendants_v[l]; if (ubnode>=K) ubnode=K; // cannot have
          // more than ubnode (="upper bound") nodes in a ST of T_{v1} union ... union T_{vl}.
          // In fact, there is a ST of T_{v1} union ... union T_{vl} with 1<=k nodes
          // (with some w) iff k<=sumoffirstnumdescendants_v[l].
          
          for (w=-1; w<=ubsize; ++w) {
            for (k=1; k<=UB[v]; ++k) { // @@ Change upper bound to ubnode?
              g_vandl[k][1+w]=-INFTY;
            }}
          // There is a 1-node summary tree with "other" = T_v1 union ... union T_{vl}:
          g_vandl[1][1+sumoffirstsizes_v[l]]=0; // This is line 1 of Lemma 3 in the writeup.
          newlist[1][1+sumoffirstsizes_v[l]].nac=0;
          
          for (w=-1; w<=ubsize; ++w) {
            wL=w-svl; // only useful if wL>=0.
            for (k=1; k<=UB[v]; ++k) { // @@ Change upper bound to ubnode?
              maxval_w[k]=-INFTY;
              casevar[k]=-INTINFTY;
            }
            maxkwithfinitecasevar=-INTINFTY; // This is reset for each w.
            oneplusw=1+w;
            if (w==-1) {
              // For line 2 of Lemma 3 in the writeup, we do the w=-1 case.
              for (k=2; k<=ubnode; ++k) {  // There is no dependence on w here, so this is not in the inner loop.
                // @@ Change ubnode?
                // Note:  Don't need k=1 since for l>=2 the only 1-node ST consists of one "other"
                // cluster, and this case is handled above.
                for (k1=1; k1<=k-1; ++k1) {
                  
                  arg1=g_vandlminus1[k1][0]; // "0" here refers to w=-1.
                  k2=k-k1;
                  combcost=comb(arg1,sss,F[vl][k2],svl);
                  // In this case, vl is active and is the root of a k2-node summary tree.
                  
                  if (combcost> maxval_w[k]) {
                    maxval_w[k]= combcost; // Here w=-1.
                    kstar[k][0]=k1;
                    casevar[k]=0;
                    if (k>maxkwithfinitecasevar) maxkwithfinitecasevar=k;
                  }
                }}}
            else { // Now w>=0.
              if (w<=sumoffirstsizes_v[lminus1]) {
                tempub1=sumoffirstnumdescendants_v[l-1]; if (tempub1>UB[v]-1) tempub1=UB[v]-1;
                // @@@ WAS K-1
                UBminusk1=UB[v]; // just initialization
                for (k1=1; k1<=tempub1; ++k1) {  // This is for case (a) in the writeup.
                  UBminusk1--; // This ensures that UBminusk1=UB[v]-k1.
                  // THIS IS THE INNER LOOP!
                  if ((arg1=g_vandlminus1[k1][oneplusw])>=0) { // No need to bother if g_vandlminus1[k1][oneplusw]=-INFTY.
                    // I rearranged the loops so that the previous line, which should speed
                    // up the code considerably, could be added.
                    k=k1; // Just initialization;  k will equal k1+k2.
                    
                    // Now let's find a good upper bound "tempub2" for k2:
                    tempub2=sumoffirstnumdescendants_v[l]-k1;
                    // because we may assume k1+k2<=sumoffirstnumdescendants_v[l].
                    if (tempub2>UBminusk1) tempub2=UBminusk1; // because we may assume k1+k2<=UB[v].
                    if (tempub2>ndvl) tempub2=ndvl;
                    //  because we may assume k2<=numdescendants[vl]
                    
                    for (k2=1; k2<=tempub2; ++k2) {
                      ++k; // so k=k1+k2
                      combcost=comb(arg1,sss,F[vl][k2],svl);
                      if (combcost>maxval_w[k]) {
                        maxval_w[k]=combcost;
                        kstar[k][oneplusw]=k1;
                        casevar[k]=1;
                        if (k>maxkwithfinitecasevar) maxkwithfinitecasevar=k;
                      } // if (combcost>
                    } // for (k2
                  } // if (g_vandlminus1[k1][oneplusw]>=0)
                } // end of inner loop.
              } // if (w<=
              
              // Now do the rest of the cases.
              for (k=2; k<=ubnode; ++k) { // @@ Is ubnode correct?
                countofsteps++;
                
                ////////////////////////////////////////////////////////////////////////////
                
                if (svl==w) {
                  arg1=g_vandlminus1[k-1][0];
                  
                  combcost=comb(arg1,sss,0,svl);
                  if (combcost>maxval_w[k]) { if (ASSERT) assert(arg1>=0);
                    maxval_w[k]=combcost;
                    kstar[k][oneplusw]=-INTINFTY;
                    casevar[k]=2;
                    if (k>maxkwithfinitecasevar) maxkwithfinitecasevar=k;
                  }
                }
                
                ////////////////////////////////////////////////////////////////////////////
                
                Hprime=-INFTY;
                W=sumoffirstsizes_v[lminus1];
                if ((wL>=0)&&(wL<=W)) {
                  wR=svl;
                  if (W+wR==0) {
                    if (ASSERT) assert((W==0)&&(wR==0));
                  }
                  else {
                    
                    if (ASSERT) assert(wL<=sss);
                    H=g_vandlminus1[k][1+wL]; // NOTE:  g_vandl with second
                    // index of 1+w relies on g_vandlminus1 with a DIFFERENT second index!
                    // This proves that the second index is necessary.
                    if (H>=0) {
                      if (ASSERT) assert(k<=sumoffirstnumdescendants_v[lminus1]);
                      if ((H<0)&&(H!=-INFTY)) {
                        //fprintf(mainoutputfile,"line 1006: H=%lf but not %lf.\n", H,-INFTY);
                        exit(-1);
                      }
                      if (ASSERT) assert(W+wR>0);
                      Hprime=(-1.0/(W+wR))*(  -W*H+xlgx(W)-xlgx(wL)-xlgx(W+wR)+xlgx(wL+wR)  );
                      if ((Hprime<0)&&(Hprime!=-INFTY)) {
                        //fprintf(mainoutputfile,"line 1013: Hprime=%lf but not %lf.\n", Hprime,-INFTY);
                        exit(-1);
                      }
                      if (W==0) if (ASSERT) assert (Hprime<=0);
                      if (Hprime>maxval_w[k]) {
                        maxval_w[k]=Hprime;
                        kstar[k][oneplusw]=-INTINFTY;
                        casevar[k]=3;
                        if (k>maxkwithfinitecasevar) maxkwithfinitecasevar=k;
                      }
                    } // if (H>=0)
                  } // else
                  // WARNING:  casevar[k] may be -INTINFTY here.  This will happen if and only if
                  // there is no k-node summary tree, having an "other" cluster of weight w for the given
                  // v,l,k,w.  This happens, e.g., in "badgreedy" for v=1, l=2, k=4, w=0.  There *is* a
                  // 4-node ST of T_v1 union T_v2, but it has no "other" node, i.e., w=-1.  There simply
                  // is no 4-node ST of T_v1 union T_v2 with an "other" cluster of weight 0.
                } // if ((wL>=0 etc.
                // if (casevar[k]==-INTINFTY) fprintf(mainoutputfile,"-1 ");
                // else fprintf(mainoutputfile,"%d ",casevar[k]);
              } // for (k=2;
              // fprintf(mainoutputfile,"\n");
            } // else "Now w>=0"
            ////////////////////////////////////////////////////////////////////////////
            
            // Now we set g_vandl[k][oneplus] and create newlist.
            for (k=2; k<=maxkwithfinitecasevar; ++k) {
              
              g_vandl[k][oneplusw]=maxval_w[k];
              
              // Try to figure out a way to avoid copying the full lists below.
              
              switch (casevar[k]) {
                  
                case -INTINFTY:
                  // In this case, since there is no k-node ST of T_v1 union ... union T_vl, we just
                  // set newlist[k][1+w].nac to be -INTINFTY, to signify the nonexistence
                  // of the desired summary tree.
                  newlist[k][1+w].nac=-INTINFTY;
                  break;
                  
                case 0: // and therefore w=-1...
                  // This is line 2 of Lemma 3 in the writeup.
                  k1=kstar[k][0]; k2=k-k1;
                  if (ASSERT) assert((1<=k1)&&(k1<=k));
                  if (ASSERT) assert((1<=k2)&&(k2<=k));
                  
                  // Since oldlist[k1][0].nac=-INFTY means that there is no k-node ST without
                  // an other component, and l>=2, oldlist[k1][0].nac in this case should be >=0:
                  length=oldlist[k1][0].nac;
                  if (ASSERT) assert((0<=length)&&(length<=k));
                  // Copy the list oldlist[k1][0].child to newlist[k][0].child, and then append vl,
                  // and copy the list oldlist[k1][0].orderofST to newlist[k][0].orderofST, and append k2=k-kstar[k][0]:
                  // fprintf(mainoutputfile,"Line 949: Case 0, v=%d, l=%d, w=%d, k=%d: Copying a list of length %d.\n",v,l,w,k,length);
                  // See below now, instead of doing this here. copy(k1,-1,k,-1); append(vl,k2,k,-1);
                  // Since length seems to be usually 1, 2, 3, or 4, speed up the copying
                  // by making it inline:
                  
                  oldk=k1; oldwprime=0; newk=k; newwprime=0;
                  switch (length) {
                      
                    case 4:
                      // @@ TRY SETTING p TO newlist[newk][newwprime].child and referring to *(p+4)=*(q+4).
                      newlist[newk][newwprime].child[4]=oldlist[oldk][oldwprime].child[4];
                      newlist[newk][newwprime].orderofST[4]=oldlist[oldk][oldwprime].orderofST[4];
                      // no "break" here, so we *will* fall through, as intended.!
                      
                    case 3:
                      newlist[newk][newwprime].child[3]=oldlist[oldk][oldwprime].child[3];
                      newlist[newk][newwprime].orderofST[3]=oldlist[oldk][oldwprime].orderofST[3];
                      // no "break" here, so we *will* fall through, as intended.!
                      
                    case 2:
                      newlist[newk][newwprime].child[2]=oldlist[oldk][oldwprime].child[2];
                      newlist[newk][newwprime].orderofST[2]=oldlist[oldk][oldwprime].orderofST[2];
                      // no "break" here.
                      
                    case 1:
                      newlist[newk][newwprime].child[1]=oldlist[oldk][oldwprime].child[1];
                      newlist[newk][newwprime].orderofST[1]=oldlist[oldk][oldwprime].orderofST[1];
                      break; // Don't do default case also.
                      
                    default:
                      copy(oldk,oldwprime,newk,newwprime);
                      
                  } // of inner switch
                  
                  // And now append (vl,k2):
                  newlist[newk][newwprime].child[length+1]=vl;
                  newlist[newk][newwprime].orderofST[length+1]=k2;
                  
                  newlist[newk][newwprime].nac=length+1;
                  if (ASSERT) assert(newlist[newk][newwprime].nac<=k);
                  
                  break; // of outer switch
                  
                  
                case 1: // Case 3a in the writeup
                  k1=kstar[k][1+w]; k2=k-k1;
                  if (ASSERT) assert((1<=k1)&&(k1<=k));
                  if (ASSERT) assert((1<=k2)&&(k2<=k));
                  // Since oldlist[k1][1+w].nac=-INTINFTY means there is no k-node ST with an "other"
                  // cluster of weight w, oldlist[k1][1+w].nac should be >=0:
                  length=oldlist[k1][1+w].nac; if (ASSERT) assert((0<=length)&&(length<=k));
                  // Copy the list oldlist[k1][1+w].child to newlist[k][1+w].child, and then append vl,
                  // and copy the list oldlist[k1][1+w].orderofST to newlist[k][1+w].orderofST,
                  // and then append k2=k-k1=k-kstar[k][1+w]:
                  // fprintf(mainoutputfile,"Line 962: Case 1, v=%d, l=%d, w=%d, k=%d: Copying a list of length %d.\n",v,l,w,k,length);
                  // copy(k1,w,k,w); // append(vl,k2,k,w);
                  if (ASSERT) assert(newlist[k][1+w].nac<=k);
                  
                  // Since length seems to be usually 1, 2, 3, or 4, speed up the copying
                  // by making it inline.:
                  
                  oldk=k1; oldwprime=1+w; newk=k; newwprime=1+w;
                  switch (length) {
                      
                    case 4:
                      newlist[newk][newwprime].child[4]=oldlist[oldk][oldwprime].child[4];
                      newlist[newk][newwprime].orderofST[4]=oldlist[oldk][oldwprime].orderofST[4];
                      // no "break" here, so we *will* fall through, as intended.!
                      
                    case 3:
                      newlist[newk][newwprime].child[3]=oldlist[oldk][oldwprime].child[3];
                      newlist[newk][newwprime].orderofST[3]=oldlist[oldk][oldwprime].orderofST[3];
                      // no "break" here, so we *will* fall through, as intended.!
                      
                    case 2:
                      newlist[newk][newwprime].child[2]=oldlist[oldk][oldwprime].child[2];
                      newlist[newk][newwprime].orderofST[2]=oldlist[oldk][oldwprime].orderofST[2];
                      // no "break" here!
                      
                    case 1:
                      newlist[newk][newwprime].child[1]=oldlist[oldk][oldwprime].child[1];
                      newlist[newk][newwprime].orderofST[1]=oldlist[oldk][oldwprime].orderofST[1];
                      break; // Don't do default case also.
                      
                    default:
                      copy(oldk,oldwprime,newk,newwprime);
                      
                  } // of inner switch
                  
                  // And now append:
                  newlist[newk][newwprime].child[length+1]=vl;
                  newlist[newk][newwprime].orderofST[length+1]=k2;
                  
                  newlist[newk][newwprime].nac=length+1;
                  if (ASSERT) assert(newlist[newk][newwprime].nac<=k);
                  
                  break;
                  
                case 2: // Case 3b in the writeup
                  if (ASSERT) assert(w==svl);
                  // In T_v1 union ... union T_v{l-1},
                  // there should be a (k-1)-node ST with no "other" cluster, so:
                  length=oldlist[k-1][0].nac;
                  if (ASSERT) assert((0<=length)&&(length<=k));
                  // Copy the list oldlist[k-1][0].child to newlist[k][1+w].child, and then append nothing,
                  // and copy the list oldlist[k-1][0].orderofST to newlist[k][1+w].orderofST, and append nothing.
                  // No node is appended since node v_l is inactive, i.e., a part of "other."  In fact, "other"=V(T_{vl}) in this case.
                  // fprintf(mainoutputfile,"Line 975: Case 2, v=%d, l=%d, w=%d, k=%d: Copying a list of length %d.\n",v,l,w,k,length);
                  // "copy(k-1,-1,k,w);" is done via the following switch:
                  
                  oldk=k-1; oldwprime=0; newk=k; newwprime=1+w;
                  switch (length) {
                      
                    case 4:
                      newlist[newk][newwprime].child[4]=oldlist[oldk][oldwprime].child[4];
                      newlist[newk][newwprime].orderofST[4]=oldlist[oldk][oldwprime].orderofST[4];
                      // no "break" here, so we *will* fall through, as intended.!
                      
                    case 3:
                      newlist[newk][newwprime].child[3]=oldlist[oldk][oldwprime].child[3];
                      newlist[newk][newwprime].orderofST[3]=oldlist[oldk][oldwprime].orderofST[3];
                      // no "break" here, so we *will* fall through, as intended.!
                      
                    case 2:
                      newlist[newk][newwprime].child[2]=oldlist[oldk][oldwprime].child[2];
                      newlist[newk][newwprime].orderofST[2]=oldlist[oldk][oldwprime].orderofST[2];
                      // no "break" here!
                      
                    case 1:
                      newlist[newk][newwprime].child[1]=oldlist[oldk][oldwprime].child[1];
                      newlist[newk][newwprime].orderofST[1]=oldlist[oldk][oldwprime].orderofST[1];
                      break; // Don't do default case also.
                      
                    default:
                      copy(oldk,oldwprime,newk,newwprime);
                      
                  } // of inner switch
                  
                  newlist[newk][newwprime].nac=length;
                  if (ASSERT) assert(newlist[newk][newwprime].nac<=k);
                  
                  break;
                  
                case 3: // Case 3c in the writeup
                  if (ASSERT) assert(w>=svl);
                  wL=w-svl;
                  length=oldlist[k][1+wL].nac;
                  if (ASSERT) assert((0<=length)&&(length<=k));
                  
                  // Copy the list oldlist[k][1+wL].child to newlist[k][1+w].child, and then append nothing,
                  // and copy the list oldlist[k][1+wL].orderofST to newlist[k][1+w].orderofST, and append nothing.
                  // fprintf(mainoutputfile,"Line 987: Case 3, v=%d, l=%d, w=%d, k=%d: Copying a list of length %d.\n",v,l,w,k,length);
                  if (ASSERT) assert(w<=ubsize);
                  // "copy(k,wL,k,w);" is done via the following switch:
                  
                  oldk=k; oldwprime=1+wL; newk=k; newwprime=1+w;
                  
                  switch (length) {
                      
                    case 4:
                      newlist[newk][newwprime].child[4]=oldlist[oldk][oldwprime].child[4];
                      newlist[newk][newwprime].orderofST[4]=oldlist[oldk][oldwprime].orderofST[4];
                      // no "break" here, so we *will* fall through, as intended.!
                      
                    case 3:
                      newlist[newk][newwprime].child[3]=oldlist[oldk][oldwprime].child[3];
                      newlist[newk][newwprime].orderofST[3]=oldlist[oldk][oldwprime].orderofST[3];
                      // no "break" here, so we *will* fall through, as intended.!
                      
                    case 2:
                      newlist[newk][newwprime].child[2]=oldlist[oldk][oldwprime].child[2];
                      newlist[newk][newwprime].orderofST[2]=oldlist[oldk][oldwprime].orderofST[2];
                      // no "break" here!
                      
                    case 1:
                      newlist[newk][newwprime].child[1]=oldlist[oldk][oldwprime].child[1];
                      newlist[newk][newwprime].orderofST[1]=oldlist[oldk][oldwprime].orderofST[1];
                      break; // Don't do default case also.
                      
                    default:
                      copy(oldk,oldwprime,newk,newwprime);
                      
                  } // of inner switch
                  
                  newlist[newk][newwprime].nac=length; // since we append nothing.
                  if (ASSERT) assert(newlist[newk][newwprime].nac<=k);
                  break; // since we don't want to do the default case here.
                  
                default:
                  //fprintf(mainoutputfile,"Line 1139: casevar[%d]=%d, which is not in {-INTINFTY,0,1,2,3}.  Exiting.\n",k,casevar[k]);
                  exit(-42);
                  
              }
              
              //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
              
              // The following "if and only if" is wrong and hence commented out.  The reason is that
              // even when k<=sumoffirstnumdescendants_v[l],g_vandl[k][oneplusw] might
              // be -INFTY.  For example, if the first l children of v are
              // isolated nodes, k<l, and w=-1, there's no way to get exactly k
              // clusters without an "other" node.
              // if (ASSERT) assert((k<=sumoffirstnumdescendants_v[l])==(maxval>=0));
              
              if (maxval_w[k]>=0) {
                if (ASSERT) assert (k<=sumoffirstnumdescendants_v[l]);
              }
              if (maxval_w[k]<0) if (ASSERT) assert(maxval_w[k]==-INFTY);
              
            } // for (k
          } // for (w
          
          //////////////////////////////////////////////////////////////////////////////////////
          
          // Now free g_vandlminus1 (for l-1), malloc g_vandlminus1 (for l), copy g_vandl to g_vandlminus1, and free g_vandl.
          for (k=1; k<=UB[v]; ++k) {
            free(g_vandlminus1[k]);
          }
          free(g_vandlminus1);
          
          g_vandlminus1=(float **) malloc ((1+K)*sizeof(float *));
          if (ASSERT) assert(g_vandlminus1!=NULL);
          // Note: We are about to mallock g_vandlminus1 for the l'th child of v, which in a moment will
          // become the (l-1)'st child of v (except when l=numchildren[v]).  If l=numchildren[v],
          // then l will continue to refer to the last child of v.
          for (k=1; k<=UB[v]; ++k) {
            g_vandlminus1[k]=(float *) malloc ((2+sumoffirstsizes_v[l])*sizeof(float));
            if (ASSERT) assert(g_vandlminus1[k]!=NULL);
          } // for (k
          
          // Initialize g_vandlminus1:
          for (k=1; k<=UB[v]; ++k) { // @@ would ubnode work?
            for (w=-1; w<=ubsize; ++w) {
              g_vandlminus1[k][1+w]=-INFTY;
            }}
          
          
          // Now copy g_vandl to g_vandlminus1.
          for (k=1; k<=ubnode; ++k) { // @@ Is this best?
            for (wprime=0; wprime<=1+ubsize; ++wprime) {
              g_vandlminus1[k][wprime]=g_vandl[k][wprime];
            }}
          
          // Now free g_vandl.
          for (k=1; k<=UB[v]; ++k) {
            free(g_vandl[k]);
          }
          free(g_vandl);
          
          ///////////////////////////////////////////////////////////////////////////////////////////
          
          // Now free oldlist:
          for (k=1; k<=UB[v]; ++k) {
            for (w=-1; w<=sumoffirstsizes_v[l-1]; ++w) {
              // oldlist was allocated at the time when "ell" was smaller by 1.
              free(oldlist[k][1+w].child);
              free(oldlist[k][1+w].orderofST);
            }
            free(oldlist[k]);
          }
          free(oldlist);
          // Why remalloc when the arrays have the same size?  Because I'm hoping to change the arrays' lengths.
          
          oldlist=(struct achildorders **) malloc ((1+K)*sizeof(struct achildorders *)); 
          if (ASSERT) assert(oldlist!=NULL);
          for (k=1; k<=UB[v]; ++k) {
            oldlist[k]=(struct achildorders *) malloc ((2+sumoffirstsizes_v[l])*sizeof(struct achildorders)); 
            // Was: oldlist[k]=(struct achildorders *) malloc ((2+size[1])*sizeof(struct achildorders)); // 
            
            if (ASSERT) assert(oldlist[k]!=NULL);
          }
          for (k=1; k<=UB[v]; ++k) { // @@ or ubnode?
            for (w=-1; w<=sumoffirstsizes_v[l]; ++w) {   // @@@@  @@@@@@@@@@@@@@@@@@@@@@@@@@ TEST
              oldlist[k][1+w].nac=-INTINFTY; // -INTINFTY is a flag meaning that there's no such tree.
              
              oldlist[k][1+w].child=(int *) malloc ((1+k)*sizeof(int)); // Can't have more than k active nodes in a k-node summary tree.
              if (ASSERT) assert(oldlist[k][1+w].child!=NULL);
              
              oldlist[k][1+w].orderofST=(int *) malloc ((1+k)*sizeof(int)); // Can't have more than k active nodes in a k-node summary tree.
              if (ASSERT) assert(oldlist[k][1+w].orderofST!=NULL);
              
            }}
          
          
          // Then copy newlist to oldlist:
          for (k=1; k<=UB[v]; ++k) { // @@ UB[v] or ubnode?
            for (w=-1; w<=ubsize; ++w) { 
              oldlist[k][1+w].nac=newlist[k][1+w].nac;
              if (ASSERT) assert(oldlist[k][1+w].nac<=k);
              for (ll=1; ll<=newlist[k][1+w].nac; ++ll) {
                oldlist[k][1+w].child[ll]=newlist[k][1+w].child[ll];
                oldlist[k][1+w].orderofST[ll]=newlist[k][1+w].orderofST[ll];
              }}}
          
          // And last, free newlist:
          for (k=1; k<=UB[v]; ++k) {
            for (w=-1; w<=ubsize; ++w) { 
              free(newlist[k][1+w].child);
              free(newlist[k][1+w].orderofST);
            }
            free(newlist[k]);
          }
          free(newlist);
          
        } // for (l
        degree=numchildren[v]; 
        
        //////////////////////////////////////////////////////////////////////
        // Use oldlist here to store a representation of the optimal trees rooted at v:
        for (k=1; k<=UB[v]; ++k) { // @@ or ubnode?
          bestlist[v][k].nac=-INTINFTY; // initialization
          bestw=-INTINFTY; bestentropy=-INFTY;
          for (w=-1; w<=sumoffirstsizes_v[degree]; ++w) { 
            if (g_vandlminus1[k][1+w]>bestentropy) {
              bestentropy=g_vandlminus1[k][1+w];
              bestw=w;
            }
          }
          // Now copy the two lists from oldlist[k][1+bestw] for future use.
          if (bestw>=-1) { // it might still be -INTINFTY
            if (ASSERT) assert(oldlist[k][1+bestw].nac>=0);
            mallocandcopytobestlist(k,bestw,v); // i.e., copy list oldlist[k][1+bestw].child to 
            // bestlist[v][k].child and copy oldlist[k][1+bestw].orderofST to bestlist[v][k].orderofST>
          }
        }
        // WARNING:  "k" here means "k children among the PROPER descendants of v!
        for (k=1; k<=UB[v]; ++k) { // @@ or ubnode?
          if (DEBUG) if (bestlist[v][k].nac>=0) {
            //fprintf(mainoutputfile,"Here is bestlist[%d][%d]:\n",v,k);
            for (ll=1; ll<=bestlist[v][k].nac; ++ll) {
              //fprintf(mainoutputfile,"node %d and its order=%d.\n",bestlist[v][k].child[ll], bestlist[v][k].orderofST[ll]);
            }
            //fprintf(mainoutputfile,"\n");
          }
        }
        
        // Free space for oldlist:
        
        for (k=1; k<=UB[v]; ++k) {
          for (w=-1; w<=sumoffirstsizes_v[degree]; ++w) {  
            free(oldlist[k][1+w].child);
            free(oldlist[k][1+w].orderofST);
          }
          free(oldlist[k]);
        }
        free(oldlist);
        
        //////////////////////////////////////////////////////////////////////
        
        
        // Now that we've processed all of v's children, "attach" the "root," v.
        F[v][1]=0;
        for (k=1; k<=UB[v]; ++k) { // @@@@ Do I ever access f_vk[k=K][1+w]? 
          // f_vk[] refers to a summary tree rooted at v itself.
          // @@ or ubnode?
          for (wprime=1+(-1); wprime<=1+size[v]; ++wprime) { 
            f_vk[wprime]=-INFTY;
          }
        }
        
        // We will now define F[v][k] for all k.
        for (k=2; k<=UB[v]; ++k) { // @@ or ubnode?
          maxvalreal=-INFTY; wstarval=-INTINFTY;
          for (w=-1; w<=sumoffirstsizes_v[degree]; ++w) {  
            arg3=g_vandlminus1[k-1][1+w]; // Note that we're using k-1, not k.
            // At this point, since we've left the "for l" loop, g_vandlminus1 refers to l=degree, 
            // not degree-1.
            f_vk[1+w]=comb(0,weight[v],arg3,sumoffirstsizes_v[degree]); // i.e., "attach" v.
            if (f_vk[1+w]>maxvalreal) {
              maxvalreal=f_vk[1+w];
              wstarval=w;
            }
          }
          F[v][k]=maxvalreal; 
          if ((maxvalreal<0)&&(maxvalreal!=-INFTY)) {
            //fprintf(mainoutputfile,"F[%d][%d]=%lf but not -INFTY.\n",v,k,F[v][k]);
            exit(-1);
          }
        }
        
        
        ///////////////////////////////////////////////////////////
        
        // @@@@@@@@@@@@@@@
        // Now compute the entropy using
        // the original weights origweights[], not the scaled weights weight[].
        // To calculate the weight of "other", subtract off sizes of active nodes.
        
        ///////////////////////////////////////////////////////////
        
        // Now free the space used by sumoffirstsizes_v and sumoffirstnumdescendants_v.\n");
        free(sumoffirstsizes_v); free(sumoffirstnumdescendants_v);
        
        // Now we free the space we used for g_vandlminus1.
        for (k=1; k<=UB[v]; ++k) {
          free(g_vandlminus1[k]); 
        } // for (k
        free(g_vandlminus1);
        
      } // else "Now process a nonleaf"
      
    } // "for v" loop
    
  } // for d=maxdepth-1 downward
  
  //fprintf(mainoutputfile,"----------------------------------------------------------------------\n");
  for (k=1; k<=K; ++k) {
    //fprintf(mainoutputfile,"opt entropy for %d-node summary tree for the ROUNDED SCALED weights is %f (not for original weights).\n",k,F[1][k]);
  }
  
  now=time(NULL); //fprintf(mainoutputfile,"The time is now %s",ctime(&now));
  
  // Now let's draw the trees and compute their entropies.  
  //outputtreefile=fopen(outputtreefilename,"w");
  //outputclusterfile=fopen(outputclusterfilename,"w");
  
  clusterweights=(float *) malloc ((1+K)*sizeof(float)); assert(clusterweights!=NULL);
  //fprintf(mainoutputfile,"\n");
  for (k=1; k<=K; ++k) {
    numclusterssofar=0;
    Rprintf("\nNow k=%d:\n",k);   
    // Use clusterweights[1..k] out of [0..K+1] now.  Reuse the space for different k's.
    //fprintf(outputtreefile,"\n\nHere is a list of the edges of the %d-node summary tree rooted at node 1: \n",k);
    drawtreeandcomputeclusterweights(k,1); // Draw an optimal k-node summary tree rooted at node 1, the root.
    assert(k==numclusterssofar);
    // Now compute the entropy of the tree.  We know that the sum of the cluster weights is origsize[1].\n");
    entropy=0;
    for (ll=1; ll<=k; ++ll) {
      entropy-=xlgx(clusterweights[ll]/origsize[1]);
    }
    // Since F[1][k] is the optimal entropy of the scaled weights, we should have |entropy-F[1][k]|<=eps.
    err=fabs(entropy-F[1][k]); 
    //fprintf(mainoutputfile,"Line 1576: err=%lf but eps=%lf.\n",err,eps);
    assert(err<=eps+0.00001); // This is not sufficient, but it is necessary.
    // The 0.000001 is added because of the possibility, especially when eps=0, of roundoff error.
    Rprintf("The entropy is %lf (differs from F[1][%d] by %lf).\n",entropy,k,err);
  }
  
  if (!exact) //fprintf(mainoutputfile,"eps was %lf.\n",eps);
  
  free(treefilename); free(weightsfilename);
  
  /////////////////////////////////////////////////////////////////////////////
  
  free(casevar);
  
  free(maxval_w);
  
  for (k=0; k<=K; ++k) {
    free(kstar[k]);
  }
  free(kstar);
  // fprintf(mainoutputfile,"Just freed kstar on line 1201.\n");
  
  //fprintf(mainoutputfile,"Returning from main.\n");
  now=time(NULL); //fprintf(mainoutputfile,"The time is now %s",ctime(&now));
  
  //return(0);
  
} // main
