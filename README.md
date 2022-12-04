
<!-- README.md is generated from README.Rmd. Please edit that file -->

# PrePost

<!-- badges: start -->

[![R-CMD-check](https://github.com/nhs-bnssg-analytics/PrePost/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nhs-bnssg-analytics/PrePost/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Background

The first question will be: analyse pre and post activity relative to
what? This ‘index event’ could arguably be anything – a GP attendance,
an A+E visit, a 999 call, a particular point in time at which some
intervention was made. From experience, many managers and clinicians all
around the system have the same question: what happens before patients
get to me, and what happens after? These are valuable questions in order
to understand the extent to which patients are using healthcare services
as intended, and to identify any interventions that could be made. For
instance, if multiple interactions are detected before some ‘index
event’ of interest, then analysis could reveal duplication and ‘bouncing
around’. Analysis of post-event activity could reveal opportunities to
improve care at the event point, e.g. providing additional information
to the patient to allay any concerns that may otherwise lead them to
seeking more healthcare afterwards. To date, we have received requests
from clinicians to evaluate activity pre and post events for children’s
A+E attendance, adult A+E head injury attendance, and hospital admission
for heart attacks. The purpose-built and reusable solution presented
here can answer those questions, in a matter of minutes, with very
little analyst input.

## A scalable and reusable solution

Rather than conduct bespoke analysis each time we are asked question
like these, there are benefits to having a scalable and reusable
solution, that can provide the required analysis at the touch of the
button. For the tool developed here, all the analyst will need to define
is: 1. Sufficient information to isolate the cohort of interest, via
specification of the index event (ultimately, a list of NHS numbers and
index event times). 2. Coverage of the pre/post activity, in terms of
the time range (how many hours to search before/after the index event)
and the care settings of interest (e.g. GP attends, outpatient
consultations, MSK community physio, 999 see/treat calls). The analyst
enters these inputs into an R script, which then automatically produces
an MS Word document containing the main results. Full results are also
available from a number of separate files also produced automatically by
the script.

## Installation

You can install the development version of PrePost from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("nhs-bnssg-analytics/PrePost")
```

## Examples

Please see the ‘Getting started’ tab for an example using synthetic
data. Examples that can be run on the actual System Wide Dataset can be
found under the ‘Articles’ tab.
