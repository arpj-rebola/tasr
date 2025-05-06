# tasr

A fully featured checker for the [WSR proof system](https://drops.dagstuhl.de/storage/00lipics/lipics-vol271-sat2023/LIPIcs.SAT.2023.22/LIPIcs.SAT.2023.22.pdf) implemented in Rust.

This tool elaborates and checks hinted proofs in the ASR format for WSR proofs.
It is complete and functional, but undocumented and abandoned.
To the best of my knowledge, it is the only proof checker (as of May 2025) that handles both
fixpoint-free trimming and substitution redundancy.

# A bit of history

This tool was developed in 2021, when I was writing [my disertation](https://repositum.tuwien.at/handle/20.500.12708/20137).
Originally this tool was to be included there, but this would have extended an already long thesis by about 40 pages,
so it was left out.

`tasr` can handle *weak substitution redundancy (WSR)*, an extension of substition-redundant clauses
that allows lemma derivation and trimming. To the best of my knowledge,
it is unique on that even four years later.

`tasr` was never publicized, since at the moment when it was written there were no use cases for substitution-redundant clauses
in SAT solving. This is [not the case anymore](https://arxiv.org/abs/2301.09757).
Either way, I will not be maintaining this tool since I am now working in more exciting projects.
I leave this here as a piece of history in SAT solving;
hopefully it will inspire other authors to finally implement fixpoint-free trimming
in their checkers.