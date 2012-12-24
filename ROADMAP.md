ROADMAP
=======

This document explains in as much details as possible the
list of planned changes and work to be done on the Ranch
project. It is non-exhaustive and subject to change. Items
are not ordered.

*   Write examples.

    Ideally we would have one complete example per folder.

    Examples should be commented. They may or may not be
    used for writing the user guides.

*   Continuous performance testing.

    Initially dubbed the Horse project, Ranch could benefit
    from a continuous performance testing tool that would
    allow us to easily compare the impact of the changes we
    are introducing, similar to what the Phoronix test suite
    allows.

*   Transport upgrades.

    Some protocols allow an upgrade from TCP to SSL without
    closing the connection. This is currently not possible
    through the Ranch API.

*   Resizing the acceptor pool.

    We should be able to add more acceptors to a pool but also
    to remove some of them as needed.

*   Add Transport:secure/0.

    Currently Ranch checks if a connection is secure by
    checking if its name is 'ssl'. This isn't a very modular
    solution,  adding an API function that returns whether
    a connection is secure would fix that issue.
