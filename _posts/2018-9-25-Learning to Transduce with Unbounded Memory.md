---
layout: post
title: "Review: Learning to Transduce with Unbounded Memory"
category: "NLP"
author_footer: false
---

I've decided to share a literature review blog post about [*Learning to Transduce with Unbounded Memory*](https://arxiv.org/abs/1506.02516) (Grefenstette et al., 2016). I've found that blog posts summarizing literature have been helpful to me in the past, and after having written this review for a class, I thought it might be useful to other people to share it on my blog.

## Summary

This paper introduces a formalism for a differentiable stack data structure. This fact that this stack is differentiable allows it to be integrated into standard neural network models, which opens up the possibility for new neural network architectures that learn how to use a stack to solve a certain task.

The linguistic intuition underlying this development is that stacks are a useful data structure for parsing hierarchical (i.e. context-free) structure. The state-of-the art model for many linguistic tasks is some variant of an LSTM, which is a model that has only unstructured memory. Thus, by explicitly structuring the memory available to the model into a stack, the model should be able to learn a richer representation of natural language syntax. In addition to hopefully improving performance, having stack memory should also have the affect of making the algorithm that the neural network is learning more interpretable. This is important because interpretability is a major challenge for neural network models.

The authors first present their formalism for a neural stack. This formalism is a major development because it allows the network to interact with an unbounded amount of structured memory while also being almost-everywhere differentiable. In this context, differentiability means that the values read off from the stack are a differentiable function of all the values that have been pushed on to it. This differentiability condition is sufficient for being able to train the neural network using existing optimization methods.

After proposing their differentiable stack formalism, the authors report results from neural stack networks applied to several toy linguistic tasks. The tasks considered are sequence copying, sequence reversal, bigram flipping, and toy languages meant to mimic SVO to SOV conversion and gender inflection in natural languages. On each of these tasks, a neural model augmented with a differentiable data structure does not just outperform the standard LSTM model, but converges to 100% accuracy.

This paper presents a linguistically motivated formalism for language tasks and show empirically that it can outperform standard methods on some toy tasks. Both the theoretical development of the architecture and the results are very interesting. They do not attempt to analyze how their networks are utilizing the differentiable data structure, which is a question that more recent research has gone on to address. Another open question is how these stack models would perform on natural language (not constructed) data sets.