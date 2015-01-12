#include <Rcpp.h>

#undef MESSAGE
#undef ERROR
#include "filereader.h"

using namespace Rcpp;

using namespace Resonance::R2E;
using namespace Resonance;

// [[Rcpp::export]]
List blockLevelRead(std::string fname)
{
  try
  {
    R2EReader reader(fname.c_str());
  
    List out;
    
    while(auto block = reader.nextItem())
    {
      auto item = block->getRoot<FileItem>();
      if(item.isStream())
      {
        auto stream = item.getStream();
        List si = List::create(
          Named("id") = stream.getId(),
          Named("name") = stream.getName().cStr(),
          Named("channels") = stream.getChannels(),
          Named("type") = static_cast<int>(stream.getType())
          );
        si.attr("class") = "StreamDescription";
        out.push_back(si);
      }
      if(item.isDataBlock())
      {
        auto datablock = item.getDataBlock();
        int stream = datablock.getStream();
        auto sdb = datablock.getBlock();
        auto created = sdb.getCreated();
        auto received = sdb.getReceived();
        
        NumericVector createdExp = NumericVector::create(0);
        createdExp[0] = *((double*)&created);
        createdExp.attr("class") = "integer64";
        NumericVector receivedExp = NumericVector::create(0);
        receivedExp[0] = *((double*)&received);
        receivedExp.attr("class") = "integer64";
        
        if(sdb.which() == StreamedBlock::DOUBLE)
        {
          int channels = sdb.getDouble().getData().size()/sdb.getDouble().getSamples();
          NumericMatrix db(sdb.getDouble().getSamples(), channels );
          auto data = sdb.getDouble().getData();
          
          int row=0, col=0;
          for(auto i = data.begin(); i!=data.end(); ++i)
          {
            db(row, col++) = *i;
            if(col>=channels) { row++; col=0; }
          }
          
          db.attr("class") = "DataBlock";
          db.attr("created") = createdExp;
          db.attr("received") = receivedExp;
          db.attr("stream") = stream;
          out.push_back(db);
        }
        if(sdb.which() == StreamedBlock::MESSAGE)
        {
          CharacterVector db(sdb.getMessage().cStr());
          db.attr("class") = "DataBlock";
          db.attr("created") = createdExp;
          db.attr("received") = receivedExp;
          db.attr("stream") = stream;
          out.push_back(db);
        }
      }
    }
    
    return out;
  }
  catch(...)
  {
    return List();
  }
}