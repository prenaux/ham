<?php
namespace tutorial;

/**
 * Autogenerated by Thrift Compiler (0.18.1)
 *
 * DO NOT EDIT UNLESS YOU ARE SURE THAT YOU KNOW WHAT YOU ARE DOING
 *  @generated
 */
use Thrift\Base\TBase;
use Thrift\Type\TType;
use Thrift\Type\TMessageType;
use Thrift\Exception\TException;
use Thrift\Exception\TProtocolException;
use Thrift\Protocol\TProtocol;
use Thrift\Protocol\TBinaryProtocolAccelerated;
use Thrift\Exception\TApplicationException;

/**
 * You can define enums, which are just 32 bit integers. Values are optional
 * and start at 1 if not supplied, C style again.
 */
final class Operation
{
    const ADD = 1;

    const SUBTRACT = 2;

    const MULTIPLY = 3;

    const DIVIDE = 4;

    static public $__names = array(
        1 => 'ADD',
        2 => 'SUBTRACT',
        3 => 'MULTIPLY',
        4 => 'DIVIDE',
    );
}

