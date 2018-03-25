# diskoheatmap
CLI application to generate heatmap images from position data.  
The application reads position records out of a file with the following format:
    
    <x-position>,<y-position><newline>

Example:

    ...
    8.117487,4.4327536
    8.180543,4.2605314
    8.243599,4.0883093
    8.748047,2.9709752
    8.811103,2.9654195
    8.874159,2.9674284
    ...

And turns them into a heatmap as a vector image:

<img src="https://raw.githubusercontent.com/nmaehlmann/diskoheatmap/master/example-data/cluster-radius-0-05.svg?sanitize=true">


### Technologies used:
- diagrams for generating SVG graphics: https://archives.haskell.org/projects.haskell.org/diagrams/
- options for parsing command line options: https://hackage.haskell.org/package/options

### Building and running:
    stack install --local-bin-path .\example-data\
    cd .\example-data\
    .\diskoheatmap-exe.exe .\2018-02-28-211643-p0.txt
A heatmap with the name "out.svg" shoud have been created.

### Options:
#### --out \<outputfilename\>
Specifies the path to which the heatmap should be saved.  
Default is: out.svg

#### --clusterRadius \<float\>
Specifies the size of the heatmap clusters.  
Default is: 0.1
Example:  

    .\diskoheatmap-exe.exe .\2018-02-28-211643-p0.txt --clusterRadius 0.1
    
 Looks like this:
 <img src="https://raw.githubusercontent.com/nmaehlmann/diskoheatmap/master/example-data/cluster-radius-0-1.svg?sanitize=true">
 
     .\diskoheatmap-exe.exe .\2018-02-28-211643-p0.txt --clusterRadius 0.05
     
 Looks like this:
 <img src="https://raw.githubusercontent.com/nmaehlmann/diskoheatmap/master/example-data/cluster-radius-0-05.svg?sanitize=true">
 
 #### --maxTemperature \<integer\>
 Specifies, how many position points have to be in one cluster to heat up this cluster to the maximum (make it all full black).  
 Default is: 10  
 Example
 
    .\diskoheatmap-exe.exe .\2018-02-28-211643-p0.txt --clusterRadius 0.1 --maxTemperature 20
 
 Looks like this:  
 <img src="https://raw.githubusercontent.com/nmaehlmann/diskoheatmap/master/example-data/max-temperature-20.svg?sanitize=true">
 
     .\diskoheatmap-exe.exe .\2018-02-28-211643-p0.txt --clusterRadius 0.1 --maxTemperature 10
 
 Looks like this:  
 <img src="https://raw.githubusercontent.com/nmaehlmann/diskoheatmap/master/example-data/max-temperature-10.svg?sanitize=true">
