#>

#include <jack/jack.h>

int
try_open_jack(void)
{
    jack_status_t status;
    jack_client_t *c = jack_client_open("x", JackNoStartServer, &status);

    if (status & JackFailure) {
        return 0;
    }

    jack_deactivate(c);
    jack_client_close(c);

    return 1;
}

<#

(define try-jack
  (foreign-lambda bool "try_open_jack"))

(if (try-jack)
    (system "jack_control stop")
    (system "jack_control start"))
