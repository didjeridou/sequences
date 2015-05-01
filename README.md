# Information

> Is there a portion of the Ebola virus' DNA that is common to all > five strains?

> How close is the Human Rhinovirus A (common cold) to other > dangerous viruses?

> Does a DNA sequence X contain a pattern Y?

These questions can be answered through computerized DNA analysis, and while it used to be costly to achieve, it is now becoming more accessible everyday. However, the quantity of data available is growing exponentially. 

**Sequences is an Ocaml program that implements Suffix array and Longest Common Prefix arrays to allow the user to answer these questions in an better time and space complexity than a naive solution.**

### 1. Installation

You need Ocaml installed prior to using this program. Good instructions for doing so can be found in O'Reilly's Real World OCaml.

```sh
$ cd <Sequences directory>
$ make c
```
You should now be able to execute the following command:
```sh
$ ./sequences.native run
```

### 2. Usage
Running the program without data
```sh
$ ./sequences.native run

==========================================================
                   SEQUENCES V 0.03.9
==========================================================

-._    _.--'¨`'--._    _.--'¨`'--._    _.--'¨`'--._    _
    '-:`.'|`|¨':-.  '-:`.'|`|¨':-.  '-:`.'|`|¨':-.  '.` :
  '.  '.  | |  | |'.  '.  | |  | |'.  '.  | |  | |'.  '.:
  : '.  '.| |  | |  '.  '.| |  | |  '.  '.| |  | |  '.  '.
  '   '.  `.:_ | :_.' '.  `.:_ | :_.' '.  `.:_ | :_.' '.
         `-..,..-'       `-..,..-'       `-..,..-'       `


[INFO] SEQUENCES indexes a DNA sequence as a suffix array
(from a CFNA file) for efficient analysis.

=== (sub) Command List ===

  commands      List the available commands
  data          List the data files in ./_data/
  exit          Exit Sequences
  load FILE     Load a CFNA file (files in ./_data/)
                No need to write './_data/' or '.cfna'
                ex: load ebola_r -> loads ./_data/ebola_r.cfna
  search STRING Search a STRING pattern in the loaded data
  lcp           Find the LCP in the loaded data
  lcp2 FILE1 FILE2     Find the LCP of two sequences

[Sequences] >
```
In [Sequences], you will have access to the following commands:

```sh
=== (sub) Command List ===

  commands      List the available commands
  data          List the data files in ./_data/
  exit          Exit Sequences
  load FILE     Load a CFNA file (files in ./_data/)
                No need to write './_data/' or '.cfna'
                ex: load ebola_r -> loads ./_data/ebola_r.cfna
  search STRING Search a STRING pattern in the loaded data
  lcp           Find the LCP in the loaded data
  lcp2 FILE1 FILE2     Find the LCP of two sequences
```

### 3. Demo
Load an ebola strain and search for a particular ATCG pattern:
```sh
[Sequences] > load ebola_t

[INFO] Sequence selected: >gi|302315369|ref|NC_014372.1| Tai Forest ebolavirus isolate Tai Forest virus/H.sapiens-tc/CIV/1994/Pauleoula-CI, complete genome...
[...MORE OUTPUT...]

[Sequences] > search ATCTTTT

Found your pattern at position 13917

[Sequences] > lcp

Found an LCP of length 20 at position 8275 and 4377
***LCP START***
TGATGAAGATTAAGAAAAAG
***LCP END***

[Sequences] > lcp2 ebola_t ebola_r

[INFO] Sequence selected: >gi|302315369|ref|NC_014372.1| Tai Forest ebolavirus isolate Tai Forest virus/H.sapiens-tc/CIV/1994/Pauleoula-CI, complete genome

[INFO] Sequence selected: >gi|22789222|ref|NC_004161.1| Reston ebolavirus isolate Reston virus/M.fascicularis-tc/USA/1989/Philippines89-Pennsylvania, complete genome

Found an LCP of length 23
***LCP START***
GATTATATTTTTTGGAAAATACC
***LCP END***

[Sequences] > exit
Exiting Sequences
```

