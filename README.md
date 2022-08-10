# EcotaxaTools
A variety of tools to process and quickly analyze data from Ecotaxa & Ecopart
This package is under heavy development. For a tutorial on it's use check out [my tutorial](https://thealexbarth.github.io/Ecotaxa_Tools_Tutorial/)
For updates to the package check out the [updates page](https://thealexbarth.github.io/Ecotaxa_Tools_Tutorial/info_updates-page.html)

## Some thoughts on this package:
This package aims to take large blocks of code which typically are used to process UVP (or other ecotaxa-friendly instruments) and simplify common tasks.\n
Much of this package was originally written as I produced code to process UVP data on my personal projects. Only after developing this package did I realize it will be likely useful to to others and started to make a more developed product. As a result, there may be some messy code and non-direct pipelines. Please let me know if you have an improvement you'd like to contribute to the code or workflow.\n\n

When writing this package, I prioritized ease-of-use. I'd like someone who is new to coding to find this package intuitive. To do this, I try to both minimize reliance on additional packages (sorry tidyverse) when possible. Additionally, this package is made for pipe-ing (|>) and thus requires R 4.2 or newer. While I previously was hesitant to adopt piping, it is only growing in popularity in the R community. Particularly for new users, piping makes clean code for quick data transformation.\n\n

For those interested, I implemented a class structure specific to ecotaxa/ecopart objects. This may be unfamiliar to many strictly R coders however it allows the workflow to be much smoother. This approach is what allows for different datatypes to be fed into the same function and still work. The class structure is likely not something a general user of this package will have to think about. However, if you are interested in developing this package further, please consider how to utilize my 'etx classes'.

Finally, I have not prioritized speed when writing this code. Generally, most users don't need data processed at lightning speeds. However, I do try to be mindful that R is very slow in iteration. I use the apply functions as much as possible. Only on very rare occasions are loops built into functions. That said, if you are processing a large number of ecopart files, concentration functions will run slowly.

If there are any issues, contact Alex Barth AB93@email.sc.edu or abarth1014@gmail.com

