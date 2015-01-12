#include "io.h"
#include <cstdint>

using namespace Resonance::R2E;

FileOutputStream::FileOutputStream(const char *fileName)
{
    fd = fopen(fileName, "wb");
    if(!fd) throw "File can't be opened";
}

FileOutputStream::~FileOutputStream()
{
    fclose(fd);
}

void FileOutputStream::write(const void *buffer, size_t size)
{
    fwrite(buffer, size, 1, fd);
}

FileInputStream::FileInputStream(const char *fileName)
{
    fd = fopen(fileName, "rb");
    if(!fd) throw "File can't be opened";
}

FileInputStream::~FileInputStream()
{
    fclose(fd);
}

size_t FileInputStream::tryRead(void *buffer, size_t minBytes, size_t maxBytes)
{
    uint8_t* pos = reinterpret_cast<uint8_t*>(buffer);
    uint8_t* min = pos + minBytes;
    uint8_t* max = pos + maxBytes;

    while(pos < min)
    {
        ssize_t n = fread(pos, 1, max - pos, fd);
        if(n == 0)
        {
            break;
        }
        pos += n;
    }

    return pos - reinterpret_cast<uint8_t*>(buffer);
}

void FileInputStream::skip(size_t bytes)
{
    fseek(fd, bytes, SEEK_CUR);
}
