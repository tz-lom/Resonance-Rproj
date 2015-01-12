#ifndef IO_H
#define IO_H

#include <kj/io.h>
#include <stdio.h>

namespace Resonance {
namespace R2E {

class FileOutputStream: public ::kj::OutputStream
{
public:
    explicit FileOutputStream(const char *fileName);
    ~FileOutputStream();

    void write(const void *buffer, size_t size) override;
private:
    FILE *fd;
};

class FileInputStream: public ::kj::InputStream
{
public:
    explicit FileInputStream(const char *fileName);
    ~FileInputStream();

    size_t tryRead(void *buffer, size_t minBytes, size_t maxBytes);
    void skip(size_t bytes);
private:
    FILE *fd;
};

}
}

#endif // IO_H
