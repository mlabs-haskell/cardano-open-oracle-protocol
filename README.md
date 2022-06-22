# Cardano open oracle protocol

## Introduction

The Cardano open oracle protocol (COOP) is a set of guidelines complemented by
an open-source SDK for publishing and consuming on-chain data using Cardano
[CIP-31](https://github.com/cardano-foundation/CIPs/blob/238ab246d74968d8123f93a013b1849a29d39a9a/CIP-0031/README.md)
reference inputs. Reference inputs allow a data provider to publish a
data point once and multiple consumers to use the data point in on-chain dApp
scripts, without interfering with each other.

The purpose of this project is to provide a simple example of how to use
reference inputs in practice, with sufficient detail and documentation to
allow developers in the Cardano ecosystem to implement this functionality
in their own projects.

Development of the COOP is led by [MLabs](https://mlabs.city/) with feedback and
direction provided by the [Orcfax](https://www.orcfax.link/about/) oracle project
which will implement the COOP on its platform.

This project was graciously funded from the Cardano Treasury in Catalyst Fund 8:
- https://cardano.ideascale.com/c/idea/402572



## Documentation

The project is described in further detail by the following documents:
- [docs/design.md](docs/design.md) — describes the design goals, options
  considered, and the rationale for the design option selected for
  implementation.
- [docs/roadmap.md](docs/roadmap.md) — breaks down the project into milestones
  that group features in a progressively expanding scope until the launch
  version, and outlines the timeline for the milestones at a high level.
