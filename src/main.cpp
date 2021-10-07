#include "prelude.hpp"

#include <fcntl.h>
#include <sys/stat.h>

#define HEADER                      \
    "\\version \"2.22.1\"\n"        \
    "\n"                            \
    "#(set-global-staff-size 28)\n" \
    "\n"                            \
    "\\paper {\n"                   \
    "  indent = 0\\mm\n"            \
    "  line-width = 140\\mm\n"      \
    "  oddFooterMarkup = ##f\n"     \
    "  oddHeaderMarkup = ##f\n"     \
    "  bookTitleMarkup = ##f\n"     \
    "  scoreTitleMarkup = ##f\n"    \
    "}\n"                           \
    "\n"

#define BODY                                    \
    "melody = \\relative c'' {\n"               \
    "  \\clef treble\n"                         \
    "  \\time 4/4\n"                            \
    "\n"                                        \
    "  a4 g4 ais4 r8 b8 cis4 e4 f4 ees16 e8.\n" \
    "}\n"                                       \
    "\n"

#define FOOTER                 \
    "\\score {\n"              \
    "  \\new Staff \\melody\n" \
    "  \\layout { }\n"         \
    "  \\midi { }\n"           \
    "}\n"

i32 main(i32 n, char** args) {
    EXIT_IF(n < 2);
    i32 file = open(args[1], O_CREAT | O_WRONLY, S_IRUSR | S_IWUSR);
    EXIT_IF(file == -1);
    EXIT_IF(write(file, HEADER, sizeof(HEADER) - 1) == -1);
    EXIT_IF(write(file, BODY, sizeof(BODY) - 1) == -1);
    EXIT_IF(write(file, FOOTER, sizeof(FOOTER) - 1) == -1);
    EXIT_IF(close(file) == -1);
    return EXIT_SUCCESS;
}
