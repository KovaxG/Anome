# 1. Single Point Classification

## Introduction

First of, we will consider a very simple problem of classification. We still have the task of classifying a list of points or values as `Normal` or `Anomaly`. Of course we will have some initial data. This data will be already classified for us. By me.

But I am not very fast at classification, therefore we should create a program that could produce similar classifications that are considered correct.

We will use a function that performs that classification. That function will need to have the following signature:

```haskell
data Classification = Normal | Anomaly

type Classifier p = p -> Classification

classify :: [p] -> Classifier p -> [Classification]
classify list classifier = classifier <$> list
```

What are our options? How do we proceed from here? Well, according to the signature of the classifier, we only have a point of type `p` to go on when deciding if a point is an anomaly or not. This is not usually the case, since by definition an anomaly is a point that is in some way different or unusual, given a set of points.

## Classifiers

Again, informally we will use the following definition for an anomaly: _"a deviation from the common rule, type, arrangement, or form."_ However, we can only consider each point by itself and nothing else.

First off, let use start from the beginning, try out some different datatypes, and try and see if some patterns emerge from this. Looking at Haskell types, the most simple type is `Void`. However there are no instances of this type, so we will not consider it, going instead straight to `()` (unit). Unit has only one single element, and that element is `()`.

An example of a classifier of unit may be the following implementation:
```haskell
unitClassifier :: Classifier ()
unitClassifier () = Normal
```
A different possible implementation is of course:
```haskell
unitClassifier :: Classifier ()
unitClassifier () = Anomaly
```

There are no other possible implementations of this classifier. Have we learned anything? Well, we have learned that given a point having a finite number of implementations, we can have a finite number of possible classifiers. In our case we had 2 possible classifiers. What if we have not one possible value, but maybe two? Let us use the `Bool` type, and see where it gets us.

```haskell
boolClassifier1 :: Classifier Bool
boolClassifier1 b = case b of
  True  -> Normal
  False -> Normal

boolClassifier2 :: Classifier Bool
boolClassifier2 b = case b of
  True  -> Normal
  False -> Anomaly

boolClassifier3 :: Classifier Bool
boolClassifier3 b = case b of
  True  -> Anomaly
  False -> Normal  

boolClassifier4 :: Classifier Bool
boolClassifier4 b = case b of
  True  -> Anomaly
  False -> Anomaly  
```
As we can see, we can create 4 separate classifiers that are all different. We can see a pattern that starts to emerge. We could have known this beforehand, since we only have 2 classification categories. We can say that if our type has *n* possible instances, we can have exactly *2^n* classifiers.

In the case of `Int`, we have all the numbers between `-9223372036854775808` and `9223372036854775807`. If we want to know how many different possible classifiers we could have, we can calculate the number of elements, and bring it to the power of 2. That is a whole lot of classifiers. We can conclude, that a brute force method is not practical, nor is it possible in this case.

One might say, that we actually never use integers in practice, and floating point numbers should be considered instead of them. We would argue, that unless we use floating point numbers with arbitrary precision, we are still actually dealing with integers.

Sensors do not produce readings with an infinite number of decimals. Usually they produce values with, say 4 decimal places. They always produce floating point numbers with four decimal places. We could just as well multiply by 10000, and consider them integers. However this is a minor point, and more of a digression, than anything else.

From a theoretical angle, we can say that potentially we can have an infinite number of instances for a given type. For example if we were dealing with real numbers.

Even worse, the type of the classifier allows for multi-dimensional data. Until now, we considered that the point `p` was a type that had a single dimension. There is nothing that says we can't have tuples for example, or even lists.

If we are dealing with time series, our data is probably formed by a tuple consisting of a timestamp and a measured value `(TimeStamp, Value)`. We now have 2 dimensional data from a single point. We could also measure more than one thing in an instant, and the output would be a timestamp and a number of other measurements or values.

How can we hope to find the right classifier in this hugely infinite multi-dimensional space? We haven't even considered other points, just one measly point. Is this task futile and is there no hope?

Maybe we will find out in the following year.

## What is the right classifier?

For simplicity let us consider that we are dealing with real numbers. We need a classifier of type `Double`, which is not ideal, but it is good enough to make a point.
```haskell
realClassifier :: Classifier Double
realClassifier d = undefined
```
We know that there are almost an infinite number of possible classifiers. But maybe we don't have to try out classifiers for eternity.

Again, the goal is to classify correctly a list of data.
