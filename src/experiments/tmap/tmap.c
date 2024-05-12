#include <stdio.h>
#include <stdlib.h>

// Define the tree data structure
typedef struct tree {
    enum { Bin, Tip } tag;
    union {
        struct { struct tree *l; struct tree *r; } bin;
        int tip;
    } data;
} tree;

// Define the tzipper data structure
typedef struct tzipper {
    enum { Top, BinL, BinR } tag;
    union {
        struct { struct tzipper *z; struct tree *r; } binl;
        struct { struct tree *l; struct tzipper *z; } binr;
    } data;
} tzipper;

tree* app(tree* t, int (*f)(int), tzipper* ctz);
tree* down(tree* t, int (*f)(int), tzipper* ctx);


// Function to create a new Tip node
tree* create_Tip(int x) {
    tree *t = (tree*)malloc(sizeof(tree));
    t->tag = Tip;
    t->data.tip = x;
    return t;
}

// Function to create a new Bin node
tree* create_Bin(tree *l, tree *r) {
    tree *t = (tree*)malloc(sizeof(tree));
    t->tag = Bin;
    t->data.bin.l = l;
    t->data.bin.r = r;
    return t;
}

// Function to create a new Top tzipper node
tzipper* create_Top() {
    tzipper *z = (tzipper*)malloc(sizeof(tzipper));
    z->tag = Top;
    return z;
}

// Function to create a new BinL tzipper node
tzipper* create_BinL(tzipper *z, tree *r) {
    tzipper *b = (tzipper*)malloc(sizeof(tzipper));
    b->tag = BinL;
    b->data.binl.z = z;
    b->data.binl.r = r;
    return b;
}

// Function to create a new BinR tzipper node
tzipper* create_BinR(tree *l, tzipper *z) {
    tzipper *b = (tzipper*)malloc(sizeof(tzipper));
    b->tag = BinR;
    b->data.binr.l = l;
    b->data.binr.z = z;
    return b;
}

// Function to apply the function to the tree while traversing up the zipper
tree* app(tree *t, int (*f)(int), tzipper *ctx) {
    if (ctx->tag == Top) {
        free(ctx);
        return t;
    } else if (ctx->tag == BinR) {
        tree* binr_l = ctx->data.binr.l;
        tzipper* binr_z = ctx->data.binr.z;
        free(ctx);
        return app(create_Bin(t, binr_l), f, binr_z);
    } else {
        tree* binl_r = ctx->data.binl.r;
        tzipper* binl_z = ctx->data.binl.z;
        free(ctx);
        return down(binl_r, f, create_BinR(t, binl_z));
    }
}

// Function to recursively traverse down the tree while applying the function
tree* down(tree *t, int (*f)(int), tzipper *ctx) {
    if (t->tag == Bin) {
        tree* l = t->data.bin.l;
        tree* r = t->data.bin.r;
        free(t);
        return down(l, f, create_BinL(ctx, r));
    } else {
        int fx = f(t->data.tip);
        free(t);
        return app(create_Tip(fx), f, ctx);
    }
}

// Function to map the function over the tree
tree* tmap(tree *t, int (*f)(int)) {
    return down(t, f, create_Top());
}

// Function to recursively calculate the sum of all tips in the tree
int fold_sum(tree *t) {
    if (t->tag == Tip) {
        int res = t->data.tip;
        free(t);
        return res;
    } else {
        int res_l = fold_sum(t->data.bin.l);
        int res_r = fold_sum(t->data.bin.r);
        free(t);
        return res_l + res_r;
    }
}

// Function to negate an integer
int negate(int n) {
    return -n;
}

// Function to recursively create a binary tree
tree* create_tree(int level) {
    if (level == 1) {
        return create_Tip(1);
    } else {
        int pred_level = level - 1;
        tree *res_l = create_tree(pred_level);
        tree *res_r = create_tree(pred_level);
        return create_Bin(res_l, res_r);
    }
}

int main() {
    tree *t = create_tree(30);
    tree *t2 = tmap(t, &negate);
    int sum = fold_sum(t2);
    printf("%d\n", sum);
    return 0;
}
