#ifndef FILEREADER_SAVELIB_H
#define FILEREADER_SAVELIB_H

#include "Messages.capnp.h"
#include "io.h"
#include <capnp/serialize-packed.h>


namespace Resonance {
namespace R2E {


class R2EReader
{
public:
    R2EReader(const char *fileName);

    ::capnp::MessageReader *nextItem();

private:
    FileInputStream file;
    ::kj::BufferedInputStreamWrapper in;
    bool packed;
};

}
}

#endif
