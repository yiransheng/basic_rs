# VM Performance Notes

I have not tried much to optimize VM performances much with typical techniques I have read here and there.

However, based on profiling, the only significant improvements I got is by adding `#inline[always]` to`VM` `struct` methods called in the execution loop. Doing this alone improves the execution time by roughly 20% (profiled by few programs).

As of now its fairly slow compared to actual programming languages, compared to python for instance (even with its iterator baggage), `basic_rs` runs at about half speed. Although it is faster than a few tree-walk interpreters I have found  at least.

Further improvement could come from one of the following:

* Optimization at IR level
* Improve VM instruction design
  * Translate global variable name to index, and use `Vec` instead of `HashMap`
* Improve rust code (remove `nan`-tagging for instance might give a 7% improvement), might even use some unsafe code



