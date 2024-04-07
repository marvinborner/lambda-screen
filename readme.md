# Lambda Screen

See the accompanying [blog
post](https://text.marvinborner.de/2024-03-25-02.html) for more
information.

## Details

-   WebGL for drawing the squares
-   1 Webworker for handling paint requests
-   single-threaded reduction via randomized stack popping (see
    [4fa7012](https://github.com/marvinborner/lambda-screen/tree/4fa7012e03e09fa62bd0080f2c7bfbf02b00a6ca)
    for an implementation using 4 parallel reduction workers; they
    crashed firefox consistently)
-   Currently no higher-order reduction
-   No libraries
