# About this project

Hello, I am Kovax, and my goal with this project is to document my research on anomaly detection in time series. I do this here because I would like to make notes and write down my experiments and thoughts, so that it will be easy to collect all my ideas at the end in the case it turns out that I have enough interesting material to write a paper about it.

## What's it about?

I have chosen to dabble in the world of anomaly detection. More specifically, anomaly detection in time series. The main goal of this research is to create a program, show it a time series, and have it tell me if there are anomalies, and if so, where are they. It would be even better if the program would correctly identify anomalies. For some definition of _correct_ anyway...

I will be using the programming language `Haskell`. It is an unusual pick for such a task, since most would use a language like `MatLab`, `R`, `python` or even `C++`, since these languages are much more associated with data science. However, I decided to go with `Haskell`, because of a personal preference of strongly typed functional languages. Also, I are researching anomalies anyway, why not be one?

Actually, what is considered an anomaly? Well that is an interesting questions. One which must be answered if we want to formally define it. And we really do. Dictionary.com defines this word as _"a deviation from the common rule, type, arrangement, or form."_. Basically we will consider an anomaly anything that is unusual, unexpected or hard to predict given some data.

We define the following tautology:   
Given a time series made up from a number of points `n`, we can classify any and all points as either a `Normal` value, or an `Anomaly`.

Of course we can not usually say whether a point for example is an anomalous point or not, simply by observing that point. It is usually a function of that point, and all other points. In other words, we say that there must exist a function `classify`, that takes a point `p`, and a list of points `[p]`, and produces a classification for the given point. In Haskell terms, we can write:

```haskell
data Classification = Normal | Anomaly

classify :: p -> [p] -> Classification
```

## Failed attempt

This is not my first attempt at doing this work. My first attempt was to develop an anomaly detection platform in `python`. Well, let's just say that I'm not a fan of dynamically typed languages for personal reasons, and leave it at that. I just couldn't make myself write code.

Another reason is that I documented my work in a bunch of different formats. I wrote stuff on paper, I wrote stuff on paper and then I scanned that paper and put it next to my notes that I wrote on the computer... It was a mess.

I didn't get any enjoyment out of it. Also, I don't like doing things I don't enjoy. Or I don't enjoy doing things I don't like. In any case, I realized that after almost a year, I didn't really have much to write home about.

## What is my plan?

My plan is to use a Github repository to document my work, and also to store my code as well as data. All will be in the same place, easily accessible from home, university or even a restaurant. Maybe also a park if I'm feeling adventurous.

I will write all my code in `Haskell`, a language that I quite fancy, and would like to practice as much as possible.

I will store all my data in a `.csv` format. I like formats that are human readable. I understand the need for saving space in large datasets, however I believe that Comma Separated Values is a much more humane way to store data.

## Anything else?

I guess I will be writing as much stuff as possible in the following year. I will add a bunch of folders, and will try to make this project as easy to navigate as possible.

If you stumble across here and have any comments, questions or thoughts on my work, feel free to leave a comment or write me an email or raise an issue. I would welcome it.

That is all.
