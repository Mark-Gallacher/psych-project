## Honours Project - Statistical Consistency in the NHST Framework

This GitHub accompanies my Dissertation to make the code available to readers.

For ease, the notebook has been divided into **4 Main Chapters**:
- **Chapter 1**: *Introduction to Consistency*
- **Chapter 2**: *Sequential and Simulated T-tests with a ROPE*
- **Chapter 3**: *Bayesian Consistency*
- **Chapter 4**: *Using Simulated ERP to Illustrate Consistency*


## Quick Summary

Science uses a Null Hypothesis to represent the absence on an effect in order
to contradict this Hypothesis to support their own. This Null Hypothesis is
typically set to zero (the mean different of zero, the correlation of zero,
...), but is it really that informative to show you didn't observe zero? 

This dissertation focussed on how we can strengthen the null hypothesis, to
make our inferences more robust and consistent in the long run. We could change
the framework, to use Bayesian Statistics instead, but this is not very
practical. We explore using a dynamic alpha level, that increases the severity
when we have more data, or defining a region around zero that represents what
would be unhelpful or impractical. First, let's go over why inferences are
tricky.

## The Problem of Inference

One of the key issues we face when conducting our experiment is that we cannot
see the entire population, we only see a small slither of the whole. If we were
studying cardiovascular disease, we are only ever studying a small group of
patients with this condition, instead of looking at every single person with
it. This means we need a way to translate our observations to this *unobserved*
population. This is called **Inference**, we assume that characteristic present
in our little group is also present in the larger group. We also take into
account that these characteristics vary across our group and across the
population. For example, maybe we are interested in the chances of recovering
from a heart attack and how a new drug might help, but different people have
different sensitivities to this drug. How can we say that the drug works for
the **population** when we *only* see the effect it has on the **sample**?

The main approach is to define a **Null Hypothesis**, a hypothesis that
represents that the drug is useless or have no biological effect. Under this
hypothesis we would observe that the individuals who received this drug would
have a very similar rate of recovery to those who received the placebo. This
goal with our study is to show that the belief in this null hypothesis is 
*unreasonable*. This is part of a bigger idea called **Falsificationism**, but
we don't need to worry about that here. To show this hypothesis is a bit silly,
we calculate the probability of observing our data assuming the hypothesis is
correct. If we expect to see no difference, but we actually observe one, we can
use this to estimate the likelihood the null hypothesis is wrong. 

When we do this, we need to define a threshold for this probability. This
defines how *surprised* we allow ourselves to be before we reject this null
hypothesis. Let's say we saw a difference that had a 40% chance of occurring
under the null, it would be hasty to say null hypothesis could not explain
this. However, if we say an effect that had a 1 in a million chance of
occurring, well it becomes more difficult to defend this null hypothesis. This
threshold of surprise is hard to pinpoint without making an arbitrary decision.
Regardless, we have walked into a new issue of never knowing if we have truly
observed a rare event or an event that *unequivocally* falsifies our
hypothesis.

## Statistical Consistency

To counter is issue of dealing with rare events, we allow ourselves to make
*some* mistakes. There are various types of mistakes, but the most important
one is called the **false positive rate**, where we reject the null hypothesis
when it is actually correct (when we are fooled by a rare event). Typically we
allow an error rate of 5%, which brings us to the core issue. We use a
**static** unchanging error rate, even when we have a lot of data. You would
expect that as the quantity and quality of data *increases*, the severity and
strictness would also *increase*. In some fields of science, this is the standard
approach, like Physics where improving the resolution of the data allows for
more precise hypotheses. 

<details>
<summary>Figure 1</summary>
"In an Inconsistent system, the number of errors do not decrease when we
increase the quantity of information. Ideally, the larger and more
expensive experiments should have a small error rate to much smaller
studies, but this is not the case." 
</details>

This concept of reducing the rate of error when our sample size increases is
called **statistical consistency**. Mathematically, its means when we approach
the infinitely large sample size our error in our inference should be tending
towards zero. This makes sense, if you measured the height of everyone in the
world, apart from one person, your sample is so representative that the jump to
describe the population is incredibly small, you would expect an absolutely
tiny amount of error. However, when the same error rate is always used, we
prevent our errors from decreasing in the cases when the null hypothesis is
genuinely the better one, like when our new drug doesn't work or the new
intervention has no effect. We would want to know quite quickly if we are
investing our time and effort into the wrong approach.

<details>
<summary>Figure 2</summary>
!["Image Showing an Consistent Framework - Error
Decreasing"](images/readme_images/p2_test.png )

In a consistent system, we are making fewer errors when our sample size
increases. Having fewer false positives, with larger samples, means the larger
studies are statistically more reliable and robust out of the box, compared to
smaller experiments.

</details>

## The Solution?

One approach would be to use a statistical framework that is statistical
consistent, like the Bayesian approach, but this is not very practical. It
would require a lot of retraining, in both conducting the analysis but also in
proper interpretation of results. More importantly, the concept of *consistency*
is a principle from the same framework that violates it, so there should be way
to achieve consistency without throwing everything away!

What we could do is define a region around zero, where if we are inside this
region, we say we have observed an effect this is not *practically* useful.
Imagine we found a drug that worked 5% of the time, this could reject the null
hypothesis because there is an effect, but it really isn't good enough to be
meaningful. This technique has many names in science, the region of
indifference, the minimum effect size of interest, but the term I am going to
use is called the **Region of Practical Equivalence** (ROPE). 

<details>
<summary>Figure 3</summary>
!["Image showing four different cases of significance when we add a
ROPE"](images/readme_images/equivalence.png)

This chart shows the mean value with some confidence intervals. With our region
around zero, the equivalence region, defined by some value *m*, we can
categorise our results are statistically significant and practically
significant. In case A, we have a strong positive result, as the mean and
confidence intervals are completely outside our equivalence region. Unlike B
and C, which have at least part of their confidence intervals inside this
region telling us the observed effect is not enough to reject the null. In case
C and D, we can actually accept the null hypothesis because they are
statistically equivalent to zero.

</details>

With our ROPE, we define a region, such as 0 +/- 0.25, that if our data falls
into it, we *hesitate* to reject the null hypothesis. Even though, we would
typically reject the null the second we have a statistically significant
result, in this case we prevent ourselves from doing do because we haven't
observed a *good enough* difference. This could mean our sample is not as
informative as we would like it to be or that the effect size we are observing
is not big enough to be of practical use.

<details>
<summary>Figure 4</summary>
!["Image showing the reduced false positive rate when we increase margin of the
ROPE"](images/readme_images/seq_rope_mean.png )

What this graph is trying to show is that the required mean difference for a
statistically significant result decreases with more data. Importantly, our
ROPE (shown with 5 different sizes) is preventing us from rejecting the null
hypothesis when the mean difference is smaller than our defined margin. Notice
how small the differences are in the first plot, are we really saying all of
these are scientifically important.

</details>

## The Other Solution? 

Another approach could be use an alpha level that changes with information
about our experiment, a *dynamic* alpha level. We could make the experimenter
predict the effect size before they begin, then input their expected sample
size, and acceptable error rates (the false positive and false negative rate)
to calculate their new alpha level. This means the *context* of the experiment
can influence the strictness of the test. We might want a strict test for
evaluation the safety of a new drug and a more lenient test when exploring the
factors that influence sleep.

This is slightly more complex, but essentially we calculate how many
observations we need to reliably observe our prediction (assuming it is true,
of course), and adjust the alpha level by some ratio between the hypothetically
required sample and our actual sample. For clarity, let's imagine we need 25
observations to observe a medium effect with a false positive and negative of 5%
and 20%, respectively. In actuality, we have 100 observations so we have more
than enough samples, for the given error rates, and we could run a more strict
experiment - one with *lower* error rates. This means as our sample size
*increases*, so too does the strictness of our test, ensuring the false positive
rate *decreases* as more information is collected. 

<details>
<summary>Figure 5</summary>
!["Overall false positive and true positive rates for various ROPEs and Dynamic
Alphas"](images/readme_images/fp_tp_grid.png )

Panel A and B show the False and True Positive Rate (FPR/TPR) for the ROPE
method. While panels C and D display FPR and TPR for the dynamic alpha
approach. There is a lot going on here, but the important thing is that both
the ROPE and the dynamic alpha technique reduce the FPR as sample sizes
increase, while the default (raw) method is much more static. We can achieve
statistical consistency but making never rejecting the null hypothesis, even
when it is wrong. To show we are not prevent ourselves from detecting a
meaningful effect, the TPR illustrates the both methods are only having a
minimal effect compared to the default (raw) strategy.

</details>


This also prevents us from rejecting the null hypothesis when we have very
large sample sizes, as even *tiny* effects can be statistically significant.
Therefore, like the ROPE which explicitly defines what effect sizes are not
useful to us, a dynamic alpha level implicitly filters these minute differences
out by ensuring they aren't statistically significant.

## Conclusion

This is more of an introduction to some of the rather technical and subtle
issues with the statistics we use in science. It is by no means comprehensive,
there are likely multiple ways of generating a dynamic alpha level, and an
equal number of arguments to abandon this style of statistics completely. It
does bring into question how effective or useful our inferences will be in the
long run, especially for the more expensive and large-scale experiments. If a
small study and a large study both carry an error rate of 5%, have we really
made the inference better? There is still no clear consensus on how to
interpret the statistic of "surprise", while the Bayesian statisticians will
point out their methods aren't troubled with these issues.

