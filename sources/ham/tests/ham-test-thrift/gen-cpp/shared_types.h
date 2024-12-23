/**
 * Autogenerated by Thrift Compiler (0.18.1)
 *
 * DO NOT EDIT UNLESS YOU ARE SURE THAT YOU KNOW WHAT YOU ARE DOING
 *  @generated
 */
#ifndef shared_TYPES_H
#define shared_TYPES_H

#include <iosfwd>

#include <thrift/Thrift.h>
#include <thrift/TApplicationException.h>
#include <thrift/TBase.h>
#include <thrift/protocol/TProtocol.h>
#include <thrift/transport/TTransport.h>

#include <functional>
#include <memory>


namespace shared {

class SharedStruct;

typedef struct _SharedStruct__isset {
  _SharedStruct__isset() : key(false), value(false) {}
  bool key :1;
  bool value :1;
} _SharedStruct__isset;

class SharedStruct : public virtual ::apache::thrift::TBase {
 public:

  SharedStruct(const SharedStruct&);
  SharedStruct& operator=(const SharedStruct&);
  SharedStruct() noexcept
               : key(0),
                 value() {
  }

  virtual ~SharedStruct() noexcept;
  int32_t key;
  std::string value;

  _SharedStruct__isset __isset;

  void __set_key(const int32_t val);

  void __set_value(const std::string& val);

  bool operator == (const SharedStruct & rhs) const
  {
    if (!(key == rhs.key))
      return false;
    if (!(value == rhs.value))
      return false;
    return true;
  }
  bool operator != (const SharedStruct &rhs) const {
    return !(*this == rhs);
  }

  bool operator < (const SharedStruct & ) const;

  uint32_t read(::apache::thrift::protocol::TProtocol* iprot) override;
  uint32_t write(::apache::thrift::protocol::TProtocol* oprot) const override;

  virtual void printTo(std::ostream& out) const;
};

void swap(SharedStruct &a, SharedStruct &b);

std::ostream& operator<<(std::ostream& out, const SharedStruct& obj);

} // namespace

#endif
