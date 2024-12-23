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
 * Structs are the basic complex data structures. They are comprised of fields
 * which each have an integer identifier, a type, a symbolic name, and an
 * optional default value.
 * 
 * Fields can be declared "optional", which ensures they will not be included
 * in the serialized output if they aren't set.  Note that this requires some
 * manual management in some languages.
 */
class Work
{
    static public $isValidate = false;

    static public $_TSPEC = array(
        1 => array(
            'var' => 'num1',
            'isRequired' => false,
            'type' => TType::I32,
        ),
        2 => array(
            'var' => 'num2',
            'isRequired' => false,
            'type' => TType::I32,
        ),
        3 => array(
            'var' => 'op',
            'isRequired' => false,
            'type' => TType::I32,
            'class' => '\tutorial\Operation',
        ),
        4 => array(
            'var' => 'comment',
            'isRequired' => false,
            'type' => TType::STRING,
        ),
    );

    /**
     * @var int
     */
    public $num1 = 0;
    /**
     * @var int
     */
    public $num2 = null;
    /**
     * @var int
     */
    public $op = null;
    /**
     * @var string
     */
    public $comment = null;

    public function __construct($vals = null)
    {
        if (is_array($vals)) {
            if (isset($vals['num1'])) {
                $this->num1 = $vals['num1'];
            }
            if (isset($vals['num2'])) {
                $this->num2 = $vals['num2'];
            }
            if (isset($vals['op'])) {
                $this->op = $vals['op'];
            }
            if (isset($vals['comment'])) {
                $this->comment = $vals['comment'];
            }
        }
    }

    public function getName()
    {
        return 'Work';
    }


    public function read($input)
    {
        $xfer = 0;
        $fname = null;
        $ftype = 0;
        $fid = 0;
        $xfer += $input->readStructBegin($fname);
        while (true) {
            $xfer += $input->readFieldBegin($fname, $ftype, $fid);
            if ($ftype == TType::STOP) {
                break;
            }
            switch ($fid) {
                case 1:
                    if ($ftype == TType::I32) {
                        $xfer += $input->readI32($this->num1);
                    } else {
                        $xfer += $input->skip($ftype);
                    }
                    break;
                case 2:
                    if ($ftype == TType::I32) {
                        $xfer += $input->readI32($this->num2);
                    } else {
                        $xfer += $input->skip($ftype);
                    }
                    break;
                case 3:
                    if ($ftype == TType::I32) {
                        $xfer += $input->readI32($this->op);
                    } else {
                        $xfer += $input->skip($ftype);
                    }
                    break;
                case 4:
                    if ($ftype == TType::STRING) {
                        $xfer += $input->readString($this->comment);
                    } else {
                        $xfer += $input->skip($ftype);
                    }
                    break;
                default:
                    $xfer += $input->skip($ftype);
                    break;
            }
            $xfer += $input->readFieldEnd();
        }
        $xfer += $input->readStructEnd();
        return $xfer;
    }

    public function write($output)
    {
        $xfer = 0;
        $xfer += $output->writeStructBegin('Work');
        if ($this->num1 !== null) {
            $xfer += $output->writeFieldBegin('num1', TType::I32, 1);
            $xfer += $output->writeI32($this->num1);
            $xfer += $output->writeFieldEnd();
        }
        if ($this->num2 !== null) {
            $xfer += $output->writeFieldBegin('num2', TType::I32, 2);
            $xfer += $output->writeI32($this->num2);
            $xfer += $output->writeFieldEnd();
        }
        if ($this->op !== null) {
            $xfer += $output->writeFieldBegin('op', TType::I32, 3);
            $xfer += $output->writeI32($this->op);
            $xfer += $output->writeFieldEnd();
        }
        if ($this->comment !== null) {
            $xfer += $output->writeFieldBegin('comment', TType::STRING, 4);
            $xfer += $output->writeString($this->comment);
            $xfer += $output->writeFieldEnd();
        }
        $xfer += $output->writeFieldStop();
        $xfer += $output->writeStructEnd();
        return $xfer;
    }
}
