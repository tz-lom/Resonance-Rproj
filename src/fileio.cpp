#include "fileio.h"
#include <cstdint>

using namespace Resonance::R2E;
#include <iostream>

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
    size_t wr = 0;
    uint8_t* pos = reinterpret_cast<uint8_t*>(const_cast<void*>(buffer));
    uint8_t* end = pos+size;
    while(wr<size)
    {
        wr += fwrite(pos, 1, end-pos, fd);
    }

}

FileInputStream::FileInputStream(const char *fileName)
{
    fd = fopen(fileName, "rb");
    if(!fd) throw "File can't be opened";
    rewind(fd);
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
        size_t n = fread(pos, 1, max - pos, fd);
        if(n == 0)
        {
            break;
        }
        pos += n;
    }

    size_t r = pos - reinterpret_cast<uint8_t*>(buffer);

    return r;
}

void FileInputStream::skip(size_t bytes)
{
    fseek(fd, bytes, SEEK_CUR);
}
