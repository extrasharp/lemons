#include <jack/jack.h>
#include <stdlib.h>

int is_jack_running(void) {
    jack_status_t status;
    jack_client_t *c = jack_client_open("x", JackNoStartServer, &status);

    if (status & JackFailure) {
        return 0;
    }

    jack_deactivate(c);
    jack_client_close(c);

    return 1;
}

int main(void) {
    if (is_jack_running()) {
        system("/usr/bin/jack_control stop");
    } else {
        system("/usr/bin/jack_control start");
    }
}
