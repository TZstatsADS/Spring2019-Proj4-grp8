# Project: OCR (Optical Character Recognition)

![image](figs/intro.png)

### [Full Project Description](doc/project4_desc.md)

Term: Spring 2019

+ Team #8
+ Team members

	+ Hu, Xinyi xh2383@columbia.edu

	+ Hu, Yiyao yh3076@columbia.edu

	+ Lu, Shuang sl4397@columbia.edu

	+ Tay, Hui Chiang ht2490@columbia.edu

	+ Wang, Tianchen tw2665@columbia.edu

+ Paper: D2[Link](https://github.com/TZstatsADS/ADS_Teaching/blob/master/Projects_StarterCodes/Project4_OCR/doc/paper/D-2.pdf) + C5[Link](https://github.com/TZstatsADS/ADS_Teaching/blob/master/Projects_StarterCodes/Project4_OCR/doc/paper/C-5.pdf)

+ Project summary: In this project, we created an OCR post-processing procedure to enhance Tesseract OCR output. We detected the incorrect words in OCR post-processing text files by using binary n-grams. Then we performed correction using topic modeling.

**Contribution statement**: ([default](doc/a_note_on_contributions.md)) All team members contributed equally in all stages of this project. All team members approve our work presented in this GitHub repository including this contributions statement.

+ Hu, Xinyi: Extracted unique words in the ground truth text files and created the ground truth words dictionary. Coded the digrams for ground truth words dictionary. Designed the OCR post-processing text detection with Yiyao Hu. Coded the OCR post-processing detection.

+ Hu, Yiyao: Worked with Xinyi Hu to finish the clean data part and build a ground truth words dictionary. Realized the detection process and designed the OCR post-processing text detection. Worked with Shuang Lu on performance measure part. Designed and realized a new evaluation measurement to measure accuracy from the character-pairs view.

+ Lu, Shuang: Implemented word-level and character-level performance measure. Worked on error detection part. Helped with integrating the final main.rmd file.

+ Tay, Hui Chiang: Did part of error correction, helped to compile main.rmd file, did presentation

+ Wang, Tianchen: Improved and finished error correction part, helped with integrating the final main.rmd file.

All members contributed during group discussion

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── lib/
├── data/
├── doc/
├── figs/
└── output/
```

Please see each subfolder for a README file.
