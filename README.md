# spawnr [![Travis-CI Build Status](https://travis-ci.com/bfatemi/spawnr.svg?token=pYv3R1FRGogyEqfAKVfU&branch=master)](https://travis-ci.com/bfatemi/spawnr)

The goal of spawnr is to quickly launch a full-scale R environment on any size DigitalOcean node using a customizable Cloud-Init yml script and DO API.

### Launch RStudio & OpenCPU Server

Launch a web accessible RStudio and OpenCPU server on a 20 CPU DigitalOcean node. DigitialOcean prices this node at 95.2 cents/hour. Desired environment should:

- Expire after 3 hours
- Have ssh access configured to github so that all my repos are available on launch
- Have all my essential packages already installed and loaded to the R environment

Thus, this awesome setup will only cost under $2.85. This amount should be easily expensible for data scientists and analysts that intermittently need computing power such as a 20cpu node that has 64GB ram. 

This interface enables two key needs: 

1. Require or prefer to conduct all the interactive analysis on a high performance computing infrustructure that is launched and ready to go without intervention
2. Require or prefer ability to send very expensive computations or data intensive tasks to an environment that can process and deliver back data, while not bottlenecking the work going on locally

The primary goal was to make this accessible to analysts and data scientists who are not experts on server setup and configuration, but require resources explore big data or analyze at scale.


```R
...
```
