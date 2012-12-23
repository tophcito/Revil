# Revil

The aim of Revil is to provide an R-based simulator for the spread of
disease, news, rumors, political views, ... Instead of
conceptionalizing each of these facets of spreading as a seperate
affair, all spreading is abstracted to the spreading of a viral
disease that turns humans into zombies.

Since popular versions of zombie viral infections assume a medical
model that is unfit for opinion analysis, a different viral model is
created and used. On top of that, also the effects of cures to the
zombie virus can be analyzed.

## Disease conceptiualization

The classic, Resident Evil concept of a virus-based zombie apocalypse
is centered around a very potent virus. The virus is not airborne and
turns humans (and animals) into aggressive and hungry zombies. These
zombies transmit the virus via biting humans. Once bitten, a human
invariably becomes a zombie. 

While this dichotomous conceptiualization of a disease has its merits
(mainly, because many diseases indeed work like that), it is less
practical when trying to understand the spreading of political views
or opinions regarding products. Here, a political attitiude like
xenophobia, can be regarded to spread like a virus. However, it would
be a very strong assumption to require the entire population being
classifiable in either xenophobs or not. Rather, the harboring of
xenophobic views must be expressed on a continous scale. 

Therefore, the viral model used is modified to reflect these
requirements. The C-Virus does not turn human into zombies, but rather
just increases their zombieness, to the point the become contagious
zombies themselves.

The medical viral model that knows only two states (healthy
vs. infected) is called the D-Virus.

The opinion viral model that knows an infinite number of states and
expresses the strength of the opinion being held is called the C-Virus.

## Features

* D-Virus and C-Virus implementation

* powerful simulation framework that allows for fine-tuning of many
  parameters

* implementation of different treatment frameworks to analyze the
  effect of counter propaganda

* various visualization modes

