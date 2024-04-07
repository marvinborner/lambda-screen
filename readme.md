# Lambda Screen

See the accompanying [blog
post](https://text.marvinborner.de/2024-03-25-02.html) for more
information.

## Implementation

-   WebGL for drawing the squares
-   1 Webworker for handling paint requests
-   4 Webworkers for reducing top-left, top-right, bottom-left, and
    bottom-right terms (using 4 priority queues)
-   Currently no higher-order reduction
-   No libraries
