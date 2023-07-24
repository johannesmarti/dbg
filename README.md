# The problem

This repository is an attempt at finding an algorithm that solves the
following decision problem: Given a finite directed graph $G$, whose
edges are labeled with $0$ and $1$, is there a natural number $n$ such
that there exists a homomorphism from the de Bruijn graph $B_n$ of
dimension $n$ to $G$?

The [de Bruijn graph](https://en.wikipedia.org/wiki/De_Bruijn_graph)
$B_n$ of dimension $n$ is defined such that its vertices are all the
binary sequences of length $n$ and for $b \in \{0,1\}$ there is a
$b$-edge from $v$ to $w$ if $v = b u$ and $w = u a$ for some binary
sequence $u$ of length $n - 1$ and some $a \in \{0,1\}$. Graphs are
understood such that between any two vertices there can be at most one
$0$-edge and at most one $1$-edge. It is possible that there is both a
$0$-edge and an $1$-edge between two nodes. Reflexive edges, which have
the same vertex as source and as target, are allowed. Homomorphisms are
assumed to preserve the labeling of the edges.

# Motivation

We study this decision problem because
1. it is beautiful, and
2. it is computationally equivalent to an open unifiability problem in
modal logic. 

See our [extended abstract](https://inria.hal.science/hal-04128087/) for
more information on the relation to unification in modal logic.

# Related code

Leif Sabellek has two repositories with related ideas:
- [Leifa/debruijn](https://github.com/Leifa/debruijn) implements some of
  the same algorithms as this repository. Additionally, it contains some
very smart code that uses a SAT-solver to check whether for a given
graph $G$ and number $n$ there is a homomorphism from $B_n$ to $G$.
- [Leifa/debruijngame](https://github.com/Leifa/debruijngame) is simple
  graphical 1-player game in which one iteratively combines vertices in
  a given graph $G$ until one has constructed a vertex with both a
  reflexive $0$-edge and a reflexive $1$-edge. If one manages to find such
  a vertex then one has proven the existence of a homomorphism from some
  $B_n$ to $G$.

# Structure of the code

The code from `src/` is structured into the following subdictionaries:

### Data

Contains basic data structures that are not necessarily related to
computing with graphs.

### Graphs

Contains multiple implementations of unlabeled and labeled graphs. All
unlabeled graphs implement `GraphInterface` and all labeled graphs
implement `LabeledGraphInterface`. These are records of functions, which
are used instead of type classes. Explicitly passing records of function
pointers, provides greater expressivity than type classes, however at
the expense of less concise code.

### Bitify

Provides code that allows us to turn graphs into a concise
bit-representation based on arbitrary-size integers.

### GraphTools

Contains additional data structures and algorithms that are useful when
computing with graphs.

### Conditions

Contains code to check two conditions on $G$ that we currently believe
to be equivalent to the existence of a homomorphism from some $B_n$ to
$G$. These conditions are that
1. $G$ satisfies the `pathCondition` from `CayleyGraph.hs`, and
2. $G$ `isConstructible` as defined in `Constructible.hs`

### HomomorphismSearch

Contains code that searches for homomorphism between graphs using an
arc-consistency procedure. In case the procedure stops without reaching
a contradiction, an arbitrary node in the domain is chosen and for all
of its possible values in the domain the procedure is started again.
This approach turned out to be less efficient than [Leif's
coding](https://github.com/Leifa/debruijn) of the homomorphism problem
using SAT-solvers.

### Lifting

This contains an implementation of the idea behind the game in
[Leifa/debruijngame](https://github.com/Leifa/debruijngame). If we find
a way of combining the right kind of vertices in the graph $G$ we have
found a homomorphism from some $B_n$ to $G$.

### Plans

Contains code to automatically compute a plan to win the game mentioned
above. This is incomplete and so far we only have a representation of
such plans, plus some code that is able to execute plans to win the
game.

### Examples

Contains examples for graphs $G$, won games and plans for winning games.

### Programs

Contains code snippets that print some useful analytic information about
graphs or search for small examples of graphs with certain properties.
This code is of very low quality but might become useful again later.
