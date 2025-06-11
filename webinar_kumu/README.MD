Data files for Kumu Webinar with ["Figures and Flowers" community](https://eolay.tilda.ws/flowersnumbers)

Original dataset Star Wars social networks is [Gabasova, E.](https://github.com/evelinag/) (2016). Star Wars social network. DOI: [https://doi.org/10.5281/zenodo.1411479](https://zenodo.org/badge/latestdoi/147848071). Dataset is converted to csv format for ease of use.

## Description of networks
Contents of the files are the following:
* `starwars-episode-N-interactions` contains the social network extracted from Episode N, where the links between characters are defined by the times the characters speak within the same scene.

* `starwars-episode-N-mentions` contains the social network extracted from Episode N, where the links between characters are defined by the times the characters are mentioned within the same scene.

* `starwars-episode-N-interactions-allCharacters` is the `interactions` network with R2-D2 and Chewbacca added in using  data from `mentions` network.

* `starwars-full-...` contain the corresponding social networks for the whole set of 6 episodes.

There are two files for each dataset, `-NODES` provides list of nodes, `-LINKS` provides list of links

### Nodes
The nodes contain the following fields:
- name: Name of the character
- value: Number of scenes the character appeared in
- colour: Colour in the visualization

### Links
Links represent connections between characters. The link information corresponds to: 
- source: name of the character on the one end of the link
- target: name of the character on the other end of the link. 
- value: Number of scenes where the “source character” and “target character” of the link appeared together.
Please not that the network is *undirected*. Which character represents the source and the target is arbitrary, they correspond only to two ends of the link.

### Citing the original dataset
If you use the dataset in your work, please use the following citation: 

