# IPI - Inflation Perception Indicator
see also [UPI](https://github.com/JonasRieger/upi/)

## Citation & References

The repository provides selected data and scripts related to the following papers.

If you're using data from this repository or want to refer to the general methodology of the IPI, please cite our paper
* [Rieger, J., Hornig, N., Schmidt, T. & Müller, H. (2023).](https://github.com/JonasRieger/mufin23/blob/master/paper.pdf) Early Warning Systems? Building Time Consistent Perception Indicators for Economic Uncertainty and Inflation Using Efficient Dynamic Modeling. Proceedings of the 3rd Workshop on Modelling Uncertainty in the Financial World ([MUFin’23](https://sites.google.com/view/w-mufin)). [GitHub](https://github.com/JonasRieger/mufin23).

Instead, if you wish to refer to a particular interpretation, please cite the follwowing paper

* Schmidt, T., Schmidt, T., Müller, H., Rieger, J. & Jentsch, C. (2026). Changing inflation perception and the propagation of news shocks. Accepted for Empirical Economics.

or the corresponding working paper

* [Müller, H., Schmidt, T., Rieger, J., Hornig, N. & Hufnagel, L.M. (2023).](http://doi.org/10.17877/DE290R-23141) The Inflation Attention Cycle: Updating the Inflation Perception Indicator (IPI) up to February 2023 - a Research Note. *DoCMA Working Paper #13*.
* [Müller, H., Rieger, J., Schmidt, T. & Hornig, N. (2022).](http://doi.org/10.17877/DE290R-22875) An Increasing Sense of Urgency: The Inflation Perception Indicator (IPI) to 30 June 2022 - a Research Note. *DoCMA Working Paper #12*.
* [Müller, H., Rieger, J., Schmidt, T. & Hornig, N. (2022).](https://doi.org/10.17877/DE290R-22769) Pressure is high - and rising: The Inflation Perception Indicator (IPI) to 30 April 2022 - a Research Note Analysis. *DoCMA Working Paper #10*.
* [Müller, H., Schmidt, T., Rieger, J., Hufnagel, L. M. & Hornig, N. (2022).](https://doi.org/10.17877/de290r-22632) A German Inflation Narrative - How the Media frame Price Dynamics: Results from a RollingLDA Analysis. *DoCMA Working Paper #9*.

See also the corresponding articles in Handelsblatt from [2022/07/25](https://www.handelsblatt.com/politik/konjunktur/nachrichten/28543406.html), [2022/05/24](https://www.handelsblatt.com/28369000.html) and [2022/03/10](https://www.handelsblatt.com/28146862.html).

## Parameters
### The parameters are likely to be adapted to the values used in Schmidt et al. (2026) in the future.
* 10 topics modeled with ``alpha=eta=0.1``
* monthly updates with six months memory
* words are taken into account as soon as they pass a threshold of five appearances in an update month
* for more details, please refer to the given scripts

## Related Software
* [rollinglda](https://github.com/JonasRieger/rollinglda) to model the rolling version of LDA.
* [ldaPrototype](https://github.com/JonasRieger/ldaPrototype) to determine a prototype from a number of runs of Latent Dirichlet Allocation.
* [tosca](https://github.com/Docma-TU/tosca) to manage and manipulate the corpora to a structure requested by ``ldaPrototype`` and to plot the corpora.
* [tmT](https://github.com/Docma-TU/tmT) to read the raw XML files of the articles and create the ``textmeta`` objects of the corpus.
* [tm](https://CRAN.R-project.org/package=tm) to preprocess the text data.
* [data.table](https://github.com/Rdatatable/data.table) to manage data tables.
* [lubridate](https://lubridate.tidyverse.org/) to handle dates.
* [ggplot2](https://ggplot2.tidyverse.org/) and
* [GGally](https://github.com/ggobi/ggally) to visualize some statistics.

## Usage
Please note: For legal reasons the repository cannot provide all data. Please [let us know](https://github.com/JonasRieger/ipi/issues) if you feel that there is anything missing that we could add. For bug reports, comments and questions please also use the [issue tracker](https://github.com/JonasRieger/ipi/issues).
