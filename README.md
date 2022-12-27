# Gossip and Push-Sum Simulation

Developed as part of coursework for COP5615 - Distributed Operating System Principles  
  
**Programming Language:** Erlang

## Description

Gossip-type algorithms can be used both for group communication and for aggregate computation. The goal of this project is to determine the convergence of such algorithms, specifically Gossip and Push-Sum, through a simulator based on actors written in Erlang. Since actors are fully asynchronous, the particular type of gossip implemented is so-called "asynchronous gossip." We implement gossip and push-sum algorithms and compare and benchmark their convergence times on various topologies. The actual network topology plays a critical role in the dissemination speed of gossip protocols. As part of this project, we have experimented with various topologies such as the full network, line, 2D grid, and imperfect 2D grid. The topology determines who is considered a neighbor in the above algorithms. The bonus implementation adds a node failure model to the simulation.

## Execution

**Compile:** ```c(project2).```   
**Execute:** ``` project2:main(N, Topology, Algorithm, T).```   
* *N - Network Size (Number of Nodes)*  
* *Topology - Topology to use for the simulation*  
* *Algorithm - Algorithm to be simulated*  
* *T - Threshold for convergence*

## Reports 
  
Main Report  
Bonus Implementation
