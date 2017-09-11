
## Answer
- 77-80: What about Bradburg et al 2016 Plos genet
**A**: Not same methods

- 348-349 + fig 3 : how do think this relative selection intensity compare to the selection coefficients that people usually state?
**A**: our simulation model do not allow to compute easilly the selection coefficients 

- 354: You could give FP rates as well, check if it is properly controlled with the BH. That is something that we wonder later when reading the results of the scan for arabidopsis
**A**: Already discuted in Helena's paper. Hypothesis testing corrected with genomic control (too much conservative).

- 356-357: might need to be argued/clarified why the simulation really represent hard sweep in ancestral population. You argued in the method that they have the same consequence (reduced diversity)  but i dont know if it’s enough. And also: would it be a hard sweep in the ancestral population or local adaptation in the already not ancestral pop.
Do the migrations start right away after the split in the simulation ?
**A**: Fdist model (M. Beaumont)

- 402: 12,701 seems a lot. If you remove linked SNPs, how many independent loci are there ?
And in the top 100 candidates, how many distinct genes are found ?
**A**: I should have done that...

-406-407 Was Horton’s scan based on Fst or another method ? Do you have an idea of the general overlap between your top 100 and his ?
**A**: just more importants genes (frigida etc...)


## Discussion
- 422: btw it seems that there is a gain in accuracy especially for the genotype frequencies G more than for Q (given Figure 1 top right panel compared to the left panels). Could you discuss that somewhere? Given how tightly linked they are it is a bit surprising and might be a wrong impression you get from the specific parameter values used for the plots?
**A**: chance

- 437: what about false positives?
**A**: not discuted

- 439-440: this is not clear to me that what you simulated is selective sweeps in ancestral populations
**A**:

- 460 multithreaded algo : does it mean you can (1)  run different K values in parallel or also that (2) you have a way to operate on eg. matrix blocks  rather than the full Q and G matrix ? When working with these kind of big dataset that you mention, the second might be needed ? What about memory limit (as we know R has issue sometimes) do you need to load the full dataset? Maybe a word on that would interest/convince the potential user.
**A**: