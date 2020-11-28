
# Utility Clustering code

## First note

The Utility Clustering code can be divided in two parts:

- One attribute code, 1D.
- Two attributes code, 2D.

The files that have the examples need the other files as source for the functions. If you run the examples it should be easy to obtain results. However, the function that returns them has inside other functions that only the source code shows. 

A more detailed view of the files is the following.

## Min and max partition 

The min and max utility clustering can be replicated in the file "Examples\_Min\_Max.R", by the function part_data(dataset,k,func,all) where $dataset$ is a vector of numeric elements, "k" is the number of clusters we want (starts with k=2), "func" is a string which was fixed "(x+2)**2" because of the utility u(x)=(x+2)^2 used in the thesis. Then, "all" is a boolean that if "True" prints all partition representations but if "False" only prints the optimal partitions, which are the ones that have both a minimum and a maximum. The code can be checked in the file "MinMax.R".

## Event partition by one attribute

The event partition simulation can be replicated by function BF_min(dataset,k,func), where "dataset" is a vector of numeric data, "k" is the number of clusters we want and "func" is a string that gives the utility of an element "x", for example, "x**2" gives the utility u(x)=x^2. Then, this function returns a plot of all loss values plus four partitions for each loss function that minimize them. To see the functions go to the file "BF\_EVENT.R" and to see the examples and create your own, go to the file "Examples\_BF\_EVENT.R".

## One attribute partition

Then, for the attribute partition with one attribute, the examples can be simulated by function data_graphs(dataset,func,k1,k2=0), where "dataset" is a vector of elements, "func" is a string that gives the utility of an element "x", "k1" is the number of clusters for both the brute force and the CART algorithms (starts with k1=1) and "k2" is the number of clusters we want to make with the CART alone (and we set at 0 by default). Thus, for example, k1=2,k2=2 makes four partitions, where k=2 and k=3 compare the algorithms but k=4 and k=5 will only call the CART. 

In the file "BF\_CART.R" there are other functions such as the brute force simulator BF(dataset,k,func,plot=T,loss_method=l1), where plot=T gives all values of loss and "loss_method" has two options, "l1" and "linf", that give the best partitions for two loss funcitons . Also, CART_met(dataset,k,func,loss_method=l1) performs the CART algorithm where loss_method has the previously explained properties. To explore the examples, see file "Examples\_BF\_CART.R".  

## Two attribute partition

To simulate the examples with two attributes, use the function data\_graphs\_2D(dataset,order,k1,k2=0,u1,u2,runtime=F), where "dataset" must be a dataframe of two attributes, whether numeric or not numeric, "order" is a dataframe that defines the attribute levels rank ,(you must put the attribute level and its rank number side by side), "k1" is the number of partitions where we compare the brute force and the CART (starts with 1), "k2" is the number of partitions to use only the CART, u1 and u2 are vectors that define the utility values for each attribute level of the first and the second attribute. Finally, "runtime" gives the plot of the time each algorithm takes to perform. See the file "Examples\_2D.R" to simulate the examples and try your own. There is also included the code for the problems of aplication. 

Furthermore, the file "BF\_CART\_2D.R" has all functions that were used, including the separated brute force and CART simulators, which are BF_2D(dataset,k,func1,func2) and CART_2D(dataset,k,func1,func2), where "func1" and "func2" are vectores that define the utility functions. There is a test version to try as well.  

## Final message

At last, please feel free to explore the code. 



