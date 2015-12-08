"""
MSP430 Emulator: This takes strings representing msp430 instructions and evaluates state.
"""
import copy
import re
import struct

class CPURegs(object):
  def __init__(self):
    self.regs = self.CreateNewRegs()
    self.special_name_mapping = {'R0': 'PC', 'R1': 'SP', 'R2': 'SR', 'R3': 'CG'}
    
  def SetReg(self, reg_name, value):
    if reg_name in self.special_name_mapping:
      reg_name = self.special_name_mapping[reg_name]
    self.regs[reg_name] = value
    
  def GetReg(self, reg_name):
    if reg_name in self.special_name_mapping:
      reg_name = self.special_name_mapping[reg_name]
    return self.regs[reg_name]
    
  def CreateNewRegs(self):
    """
    Create a clean set of initialized (set to zero) registers
    """
    #R0 is PC
    #R1 is SP
    #r2 is SR
    #R3 is CG
    regs = {'PC': 0x0000, 'SP': 0x0000, 'SR': 0x0000,  'CG':  0x0000,
            'R4': 0x0000, 'R5': 0x0000, 'R6': 0x0000,  'R7':  0x0000,
            'R8': 0x0000, 'R9': 0x0000, 'R10': 0x0000, 'R11': 0x0000, 
            'R12': 0x0000,'R13': 0x0000,'R14': 0x0000, 'R15': 0x0000}
    return regs

    

class CPUState(object):
  """
  container class to update registers and memory
  """
  def __init__(self):
    self.flag_mask = {'C':   0, # (1 << 0) == 1st bit
                      'Z':   1, # (1 << 1) == 2nd bit
                      'N':   2, # (1 << 2) == 3rd bit
                      'OFF': 4, # (1 << 4) == 5th bit
                      'V':   8} # (1 << 8) == 8th bit

    self.regs = CPURegs()
    # create blank memory
    self.memory = bytearray([0x00 for i in xrange(0xffff)])
    
  def GetRegNames(self):
    return self.regs.regs.keys()
    
  def ReadReg(self, reg_name):
    """
    Get the value of a register.
    """
    return self.regs.GetReg(reg_name)
  
  def SetReg(self, reg_name, value):
    """
    Set a register to the supplied value.
    """
    self.regs.SetReg(reg_name, value & 0xffff)
  
  def ReadMem(self, address, size):
    """
    Read size bytes from memory
    """
    if size > 1:
      return struct.unpack('<H', str(self.memory[address:address + size]))[0]
    else:
      return self.memory[address]
  
  def WriteMem(self, address, value):
    """
    Write Memory.
    
    Args:
      address: an integer between 0 and 0xffff
      value: the value to write (either one byte or two) as a string
    
    Returns:
      None
    """
    if address < 0 or address > 0xffff:
      raise MSP430InstructionEvaluatorError("Invalid address %x supplied" % address)
    if len(value) == 1:
      self.memory[address] = value
    elif len(value) == 2:
      self.memory[address:address + len(value)] = value
    else:
      raise MSP430InstructionEvaluatorError("Invalid memory value (%s) specified" % str(value))
  
  def UpdateFlags(self, flag_name, value):
    """
    Update the status register sr according to the flag and the new value
    
    Args:
      flag_name: a single character string that's one of 'C','V','N','Z'
      value: a boolean 
      
    Returns:
      None
    """
    if value == True:
      sr = self.ReadReg('SR') | (1 << self.flag_mask[flag_name])
    else:
      sr = self.ReadReg('SR') & ~((1 << self.flag_mask[flag_name] % 0xffff))
    self.SetReg('SR', sr)
  
  def IsFlagSet(self, flag_name):
    """
    Return true if the flag is set. False otherwise.
    """
    return (self.regs.GetReg('SR') & (1 << self.flag_mask[flag_name])) != 0
  

class MSP430InstructionEvaluatorError(Exception):
  pass


IMMEDIATE_OP_TYPE = "IMMEDIATE_TYPE"
ADDRESS_OP_TYPE = "ADDRESS_TYPE"
REG_OP_TYPE = "REG_TYPE"
SYMBOLIC_REG_OP_TYPE = "SYMBOLIC_TYPE"
DEREFERNCE_OP_TYPE = "DEREF_TYPE"
  

class MSP430InstructionEvaluator(object):
  """
  class that parses and evaluates msp430 instructions.
  """
  def __init__(self):
    self.mnemonic_handlers = {"ADC": self.Adc,
                              "ADD": self.Add,
                              "ADDC": self.Addc,
                              "AND": self.And,
                              "BIC": self.Bic,
                              "BIS": self.Bis,
                              "BIT": self.Bit,
                              "BR": self.Br,
                              "CALL": self.Call,
                              "CLR": self.Clr,
                              "CLRC": self.ClrC,
                              "CLRN": self.ClrN,
                              "CLRZ": self.ClrZ,
                              "CMP": self.Cmp,
                              "DADC": self.DADC,
                              "DADD": self.DADD,
                              "DEC": self.Dec,
                              "DECD": self.Decd,
                              "DINT": self.Dint,
                              "EINT": self.Eint,
                              "INC": self.Inc,
                              "INCD": self.Incd,
                              "INV": self.Inv,
                              "JC": self.Jc,
                              "JHS": self.Jc,
                              "JEQ": self.Jz,
                              "JZ": self.Jz,
                              "JGE": self.Jge,
                              "JL": self.Jl,
                              "JMP": self.Jmp,
                              "JN": self.Jn,
                              "JNC": self.Jnc,
                              "JLO": self.Jnc,
                              "JNE": self.Jnz,
                              "JNZ": self.Jnz,
                              "MOV": self.Mov,
                              "NOP": self.Nop,
                              "POP": self.Pop,
                              "PUSH": self.Push,
                              "RET": self.Ret,
                              "RETI": self.Nop,
                              "RLA": self.Rla,
                              "RLC": self.Rlc,
                              "RRA": self.Rra,
                              "RRC": self.Rrc,
                              "SBC": self.Sbc,
                              "SETC": self.SetC,
                              "SETN": self.SetN,
                              "SETZ": self.SetZ,
                              "SUB": self.Sub,
                              "SUBC": self.Subc,
                              "SWPB": self.Swpb,
                              "SXT": self.Sxt,
                              "TST": self.Tst,
                              "XOR": self.Xor}
    
  def GetRegister(self, cpu, reg_name):
    return cpu.ReadReg(reg_name)
  
  def SetRegister(self, cpu, reg_name, value):
    cpu.SetReg(reg_name, value)
    return cpu
  
  def ReadMem(self, cpu, address, size):
    return cpu.ReadMem(address, size)
  
  def WriteMem(self, cpu, address, value):
    cpu.WriteMem(address, value)
    return cpu
  
  def OperandType(self, operand_st):
    """
    Return a TYPE string.
    
    Args:
      operand_st: a token
      
    Returns:
      a string type.
    """
    operand_st = operand_st.strip()
    if operand_st.startswith("#"):
      return IMMEDIATE_OP_TYPE
    elif operand_st.startswith("R") or operand_st in ("PC", "SR", "CG", "SP"):
      return REG_OP_TYPE
    elif operand_st.startswith("&"):
      return ADDRESS_OP_TYPE
    elif operand_st.startswith("@"):
      return SYMBOLIC_REG_OP_TYPE
    elif re.search("[0123456789ABCDEF]{0,4}\(R.+\)$", operand_st):
      return DEREFERNCE_OP_TYPE
    else:
      raise MSP430InstructionEvaluatorError("ERROR: Unknown token type (%s)" % operand_st)
    
  def ReadSymbolicRegister(self, cpu, size, operand_str):
    """
    Read memory from an symbolic register operand
    """
    reg_name = operand_str.replace('@','').strip()
    return self.ReadMem(cpu, self.GetRegister(cpu, reg_name), size)
  
  def WriteSymbolicRegister(self, cpu, operand_str, value):
    """
    Write
    """
    reg_name = operand_str.replace('@', '').strip()
    return cpu.WriteMem(self.GetRegister(cpu, reg_name), value)
  
  def ReadMemDereference(self, cpu, operand_str, size):
    """
    Process a memory dereference e.g. 0(R1) or (R1)
    """
    match = re.search("([0123456789ABCDEF]{0,4})\((R.+)\)$", operand_str,
                      re.IGNORECASE)
    if match.group(1):
      offset = int(match.group(1), 16)
    else:
      offset = 0
    base_reg = match.group(2)
    
    return self.ReadMem(cpu, offset + self.GetRegister(cpu, base_reg), size)
  
  def WriteMemDereference(self, cpu, operand_str, value):
    """
    Write to a memory location addressed by a dereference e.g. A(R4) or 0(R10)
    """
    match = re.search("([0123456789ABCDEF]{0,4})\((R.+)\)$", operand_str,
                      re.IGNORECASE)
    if match.group(1):
      offset = int(match.group(1), 16)
    else:
      offset = 0
    base_reg = match.group(2)
    
    return self.WriteMem(cpu, offset + self.GetRegister(cpu, base_reg), value)
  
  def GetOpValue(self, cpu, src_operand, byte_mode=False):
    src_op_type = self.OperandType(src_operand)
    if src_op_type == IMMEDIATE_OP_TYPE:
      val =  int(src_operand.replace('#','').replace('0x',''), 16) & 0xffff
      if byte_mode:
        val = val & 0xff
      return val
    elif src_op_type == REG_OP_TYPE and byte_mode == False:
      return self.GetRegister(cpu, src_operand)
    elif src_op_type == REG_OP_TYPE and byte_mode == True:
      return self.GetRegister(cpu, src_operand) & 0x00ff
    elif src_op_type == ADDRESS_OP_TYPE and byte_mode == False:
      return self.ReadMem(cpu, int(src_operand, 16), 2)
    elif src_op_type == ADDRESS_OP_TYPE and byte_mode == True:
      addr = src_operand.replace('&', '')
      return self.ReadMem(cpu, int(addr, 16), 1)
    elif src_op_type == SYMBOLIC_REG_OP_TYPE and byte_mode == False:
      return self.ReadSymbolicRegister(cpu, 2, src_operand)
    elif src_op_type == SYMBOLIC_REG_OP_TYPE and byte_mode == True:
      return self.ReadSymbolicRegister(cpu, 1, src_operand)
    elif src_op_type == DEREFERNCE_OP_TYPE and byte_mode == False:
      return self.ReadMemDereference(cpu, src_operand, 2)
    elif src_op_type == DEREFERNCE_OP_TYPE and byte_mode == True:
      return self.ReadMemDereference(cpu, src_operand, 1)
    else:
      raise MSP430InstructionEvaluatorError("invalid operand (%s)" % src_operand)
    
  def UpdateFlagsForOperand(self, cpu, mask_str, byte_mode, dst_op, result):
    """
    Update the status register according to resulting operand.
    
    Args:
      cpu: a MSP430 
      mask_str: a string 
      btye_mode: 
      result: the result being written back to the dst operand.
      
    Returns:
      None
      
    Side affect:
      Updates SR
    """
    if 'C' in mask_str:
      if (result > 0xffff and byte_mode == False) or (result > 0xff and byte_mode):
        cpu.UpdateFlags('C', True)
      else:
        cpu.UpdateFlags('C', False)
    if 'Z' in mask_str:
      if ((result & 0xffff) == 0 and byte_mode == False) or ((result & 0xff) == 0 and byte_mode):
        cpu.UpdateFlags('Z', True)
      else:
        cpu.UpdateFlags('Z', False)
    if 'N' in mask_str:
      if ((result & (1 << 14)) == (1 << 14) and byte_mode == False) or \
         ((result & (1 << 7) == (1 << 7) )and byte_mode):
        cpu.UpdateFlags('N', True)
      else:
        cpu.UpdateFlags('N', False)
        
    if 'V' in mask_str:
      #if (srcNegative && oldDstNegative && !dstNegative) 
      oldDstValue = self.GetOpValue(cpu, dst_op, byte_mode)
      
      if byte_mode == False:
        oldDstNegative = (oldDstValue & 0x8000) != 0
        dstNegative = (result & 0x8000) != 0
      else:
        oldDstNegative = (oldDstValue & 0x80) != 0
        dstNegative = (result & 0x80) != 0
        
      if (oldDstNegative ^ dstNegative) == True:
        cpu.UpdateFlags('V', True)
      else:
        cpu.UpdateFlags('V', False)
        
  def WriteDstOperand(self, cpu, dst_op, byte_mode, result):
    # make sure result is trimmed to 2-bytes
    result = result & 0xffff
    dst_type = self.OperandType(dst_op)
    
    if dst_type == REG_OP_TYPE:
      self.SetRegister(cpu, dst_op, result)
    elif dst_type == SYMBOLIC_REG_OP_TYPE:
      if byte_mode == False:
        result = struct.pack('<H', result)
      else:
        result = struct.pack('<B', result)
      self.WriteSymbolicRegister(cpu, dst_op, result)
    elif dst_type == ADDRESS_OP_TYPE:
      addr = int(dst_op.replace('&', ''), 16)
      if byte_mode == False:
        self.WriteMem(cpu, addr, struct.pack('<H', result))
      else:
        self.WriteMem(cpu, addr, struct.pack('<B', result))
    elif dst_type == DEREFERNCE_OP_TYPE:
      if byte_mode == False:
        result = struct.pack('<H', result)
      else:
        result = struct.pack('<B', result)

      self.WriteMemDereference(cpu, dst_op, result)
    else:
      raise MSP430InstructionEvaluatorError("Invalid dst operand %s" % dst_op)
  
  def EvaluateInstruction(self, inst_string, state):
    """
    Parse and evaluate an MSP 430 instruction.
    
    Args:
      inst_string: a string containing a 
      state:  A CPUState instance
      
    Returns:
      An updated state Object
    """
    new_state = copy.deepcopy(state)
    inst = [token.replace(',','') for token in inst_string.split()]
    mnemonic = inst[0].replace('.B', '').replace('.W', '')
    self.mnemonic_handlers[mnemonic](new_state, *inst)
    
    return new_state 
  
  def Mov(self, cpu, *args):
    mnemonic, src_op, dst_op = args 
    byte_mode = mnemonic.endswith('.B')
    
    op1 = self.GetOpValue(cpu, src_op, byte_mode)
    # do the work
    result = op1
    # we don't update flags with a move
    self.WriteDstOperand(cpu, dst_op, byte_mode, result)
    
  def Adc(self, cpu, *args):
    """
    The Add carry bit instruction. (emulated instruction)
    """
    mnemonic, dst_op = args 
    byte_mode = mnemonic.endswith('.B')
    
    new_mnemonic = "ADD"
    if byte_mode:
      new_mnemonic += ".B"
    else:
      new_mnemonic += ".W"
    if cpu.IsFlagSet('C'):
      self.Add(cpu, new_mnemonic, "#1", dst_op)
    else:
      self.Add(cpu, new_mnemonic, "#0", dst_op)
  
  def Add(self, cpu, *args):
    """
    Handle ADD.W and ADD.B instructions.
    """
    mnemonic, src_op, dst_op = args 
    byte_mode = mnemonic.endswith('.B')
    
    src_val = self.GetOpValue(cpu, src_op, byte_mode)
    dst_val = self.GetOpValue(cpu, dst_op, byte_mode)
    
    # do the work of the operand
    result = src_val + dst_val
    
    self.UpdateFlagsForOperand(cpu, 'CZNV', byte_mode, dst_op, result)

    if byte_mode:
      result = result & 0x00ff
      
    self.WriteDstOperand(cpu, dst_op, byte_mode, result)
    
  def Addc(self, cpu, *args):
    """
    Handle Add with carry instructions.
    """
    mnemonic, src_op, dst_op = args 
    byte_mode = mnemonic.endswith('.B')
    
    src_val = self.GetOpValue(cpu, src_op, byte_mode)
    dst_val = self.GetOpValue(cpu, dst_op, byte_mode)
    
    # do the work of the operand
    if cpu.IsFlagSet('C'):
      result = src_val + dst_val + 1
    else:
      result = src_val + dst_val
    
    self.UpdateFlagsForOperand(cpu, 'CZNV', byte_mode, dst_op, result)
    
    if byte_mode:
      result = result & 0x00ff

    self.WriteDstOperand(cpu, dst_op, byte_mode, result)
    
  def And(self, cpu, *args):
    """
    Handle logical and instructions.
    """
    mnemonic, src_op, dst_op = args 
    byte_mode = mnemonic.endswith('.B')
    
    src_val = self.GetOpValue(cpu, src_op, byte_mode)
    dst_val = self.GetOpValue(cpu, dst_op, byte_mode)
    
    # do the work of the operand
    result = src_val & dst_val
    
    if byte_mode:
      result = result & 0x00ff
      
    self.UpdateFlagsForOperand(cpu, 'CZNV', byte_mode, dst_op, result)
    cpu.UpdateFlags('V', False)
    self.WriteDstOperand(cpu, dst_op, byte_mode, result)
    
  def Bic(self, cpu, *args):
    """
    Handle bit clear instructions.
    """
    mnemonic, src_op, dst_op = args 
    byte_mode = mnemonic.endswith('.B')
    
    src_val = self.GetOpValue(cpu, src_op, byte_mode)
    dst_val = self.GetOpValue(cpu, dst_op, byte_mode)
    
    # do the work of the operand
    result = ~src_val & dst_val
    
    if byte_mode:
      result = result & 0x00ff
      
    self.WriteDstOperand(cpu, dst_op, byte_mode, result)
    
  def Bis(self, cpu, *args):
    """
    Handle set bits instructions
    """
    mnemonic, src_op, dst_op = args 
    byte_mode = mnemonic.endswith('.B')
    
    op1 = self.GetOpValue(cpu, src_op, byte_mode)
    op2 = self.GetOpValue(cpu, dst_op, byte_mode)
    
    # do the work of the operand
    result = op1 | op2
    
    if byte_mode:
      result = result & 0x00ff
      
    self.UpdateFlagsForOperand(cpu, 'CZNV', byte_mode, dst_op, result)
    self.WriteDstOperand(cpu, dst_op, byte_mode, result)
    
  def Bit(self, cpu, *args):
    """
    Handle bit test instructions.
    """
    mnemonic, src_op, dst_op = args 
    byte_mode = mnemonic.endswith('.B')
    
    op1 = self.GetOpValue(cpu, src_op, byte_mode)
    op2 = self.GetOpValue(cpu, dst_op, byte_mode)
    
    # do the work of the operand
    result = op1 & op2
    
    if byte_mode:
      result = result & 0x00ff
      
    self.UpdateFlagsForOperand(cpu, 'CZNV', byte_mode, dst_op, result)
    cpu.UpdateFlags('V', False)
    
  def Br(self, cpu, *args):
    """
    Handle a Branch instruction. These are emulated move insts.
    """
    mnemonic, dst_op = args
    self.Mov(cpu, "MOV.W", dst_op, "PC")
    
  def Call(self, cpu, *args):
    """
    Handle a call instruciton.
    """
    mnemonic, dst_op = args
    saved_pc = self.GetRegister(cpu, "PC") + 2
    self.SetRegister(cpu, "SP", self.GetRegister(cpu, "SP") - 2)
    self.WriteMem(cpu, self.GetRegister(cpu, "SP"), struct.pack('<H', saved_pc))
    new_pc = self.GetOpValue(cpu, dst_op)
    self.SetRegister(cpu, "PC", new_pc)
    
  def Clr(self, cpu, *args):
    """
    Handle clear instrucitons.
    """
    mnemonic, dst_op = args 
    byte_mode = mnemonic.endswith('.B')
    
    new_mnemonic = "MOV"
    if byte_mode:
      new_mnemonic += ".B"
    else:
      new_mnemonic += ".W"
    self.Mov(cpu, new_mnemonic, "#0", dst_op)
    
  def ClrC(self, cpu, *args):
    """
    Handle Clear carry flag instructions.
    """
    cpu.UpdateFlags('C', False)
    
  def ClrN(self, cpu, *args):
    """
    Handle clear negative flag.
    """
    cpu.UpdateFlags('N', False)
    
  def ClrZ(sefl, cpu, *args):
    """
    Handle clear zero flag.
    """
    cpu.UpdateFlags('Z', False)
    
  def Cmp(self, cpu, *args):
    """
    Handle compare instructions.
    """
    mnemonic, src_op, dst_op = args 
    byte_mode = mnemonic.endswith('.B')
    
    op1 = self.GetOpValue(cpu, src_op, byte_mode)
    op2 = self.GetOpValue(cpu, dst_op, byte_mode)
    
    # do the work of the operand
    result = op2 + ((~op1 & 0xffff) + 1)

    self.UpdateFlagsForOperand(cpu, 'CZNV', byte_mode, dst_op, result)
 
  def bcd2i(self, bcd):
    decimalMultiplier = 1;
    digit = None
    
    i = 0;
    
    while bcd > 0:
        digit = bcd & 0xF;
        i += digit * decimalMultiplier;
        decimalMultiplier *= 10;
        bcd >>= 4;
    return i;
  
  def i2bcd(self, i):
    binaryShift = 0
    bcd = 0
    
    while i > 0:
      digit = i % 10;
      bcd += (digit << binaryShift)
      binaryShift += 4
      i /= 10
      
    return bcd
      
  def DADC(self, cpu, *args):
    """
    Decimal add carrry (not sure how this works treating as a nop for now.
    """
    mnemonic, dst_op = args 
    byte_mode = mnemonic.endswith('.B')
    
    new_nemonic = "DADD"
    
    if byte_mode:
      new_nemonic += ".B"
    
    self.DADD(cpu, new_nemonic, "#0", dst_op)
  
  def DADD(self, cpu, *args):
    """
    Decimal add (not sure how this works treating as a nop for now)
    """
    mnemonic, src_op, dst_op = args 
    byte_mode = mnemonic.endswith('.B')
    
    src_val = self.GetOpValue(cpu, src_op, byte_mode)
    dst_val = self.GetOpValue(cpu, dst_op, byte_mode)
    
    if cpu.IsFlagSet('C'):
      c = 1
    else:
      c = 0
    
    result = self.i2bcd(self.bcd2i(src_val) + self.bcd2i(dst_val) + c)
    
    self.UpdateFlagsForOperand(cpu, 'CZNV', byte_mode, dst_op, result)
    
    if byte_mode:
      result = result & 0x00ff
    self.WriteDstOperand(cpu, dst_op, byte_mode, result)
    
  
  def Dec(self, cpu, *args):
    """
    Decriment instruction.
    """
    mnemonic, dst_op = args 
    byte_mode = mnemonic.endswith('.B')
    
    op1 = self.GetOpValue(dst_op)
    
    result = op1 - 1 & 0xffff
    self.UpdateFlagsForOperand(cpu, 'CZNV', byte_mode, dst_op, result)
    if byte_mode:
      result = result & 0x00ff
    self.WriteDstOperand(cpu, dst_op, byte_mode, result)
  
  def Decd(self, cpu, *args):
    """
    Double decrement instruction.
    """
    mnemonic, dst_op = args 
    byte_mode = mnemonic.endswith('.B')
    
    op1 = self.GetOpValue(dst_op)
    
    result = op1 - 2 & 0xffff
    self.UpdateFlagsForOperand(cpu, 'CZNV', byte_mode, dst_op, result)
    if byte_mode:
      result = result & 0x00ff
    self.WriteDstOperand(cpu, dst_op, byte_mode, result)
    
  def Dint(self, cpu, *args):
    """
    Disable interrupts instruction.
    """
    mnemonic = args
    self.Bic(cpu, "BIC", "#8", "SR")
  
  def Eint(self, cpu, *args):
    """
    Enable interrupts instruction.
    """
    self.Bis(cpu, "BIS", "#8", "SR")
  
  def Inc(self, cpu, *args):
    """
    Increment instruction.
    """
    mnemonic, dst_op = args 
    byte_mode = mnemonic.endswith('.B')
    
    op1 = self.GetOpValue(cpu, dst_op, byte_mode)
    
    result = op1 + 1
    self.UpdateFlagsForOperand(cpu, 'CZNV', byte_mode, dst_op, result)
    
    if byte_mode:
      result = result & 0x00ff
      
    self.WriteDstOperand(cpu, dst_op, byte_mode, result)
   
  def Incd(self, cpu, *args):
    """
    Double increment instruciton.
    """
    mnemonic, dst_op = args 
    byte_mode = mnemonic.endswith('.B')
    
    op1 = self.GetOpValue(cpu, dst_op, byte_mode)
    
    result = op1 + 2
    self.UpdateFlagsForOperand(cpu, 'CZNV', byte_mode, dst_op, result)
    
    if byte_mode:
      result = result & 0x00ff
      
    self.WriteDstOperand(cpu, dst_op, byte_mode, result)
    
  def Inv(self, cpu, *args):
    """
    Invert instruciton.
    """
    mnemonic, dst_op = args 
    byte_mode = mnemonic.endswith('.B')
    
    op1 = self.GetOpValue(cpu, dst_op, byte_mode)
    
    result = ~op1
    self.UpdateFlagsForOperand(cpu, 'CZNV', byte_mode, dst_op, result)
    
    if byte_mode:
      result = result & 0x00ff
      
    self.WriteDstOperand(cpu, dst_op, byte_mode, result)
    
  def JumpIfCondition(self, cpu, condition, args):
    """
    Jump if a condition if a flag is set.
    """
    mnemonic, dst_op = args 
    new_pc = self.GetOpValue(cpu, dst_op)
    if condition(cpu):
      self.SetRegister(cpu, 'PC', new_pc)
    else:
      self.SetRegister(cpu, 'PC', self.GetRegister(cpu, 'PC') + 2)
    
  def Jc(self, cpu, *args):
    """
    Jump if Carry flag is set.
    """
    self.JumpIfCondition(cpu, lambda cpu: cpu.IsFlagSet('C') == True, args)
    
  def Jnc(self, cpu, *args):
    """
    Jump if Carry flag is not set.
    """
    self.JumpIfCondition(cpu, lambda cpu: cpu.IsFlagSet('C') == False, args)
      
  def Jz(self, cpu, *args):
    """
    Jump if zero (Z) is set aka jump if equal.
    """
    self.JumpIfCondition(cpu, lambda cpu: cpu.IsFlagSet('Z') == True, args)
    
  def Jnz(self, cpu, *args):
    """
    Jump if zero (Z) is not set (JNZ) aka jump if not equal (JNE).
    """
    self.JumpIfCondition(cpu, lambda cpu: cpu.IsFlagSet('Z') == False, args)
   
  def Jn(self, cpu, *args):
    """
    Jump if negative (N) is set.
    """
    self.JumpIfCondition(cpu, lambda cpu: cpu.IsFlagSet('N') == True, args)
    
  def Jge(self, cpu, *args):
    """
    Jump if greater than or equal.
    """
    condition =  lambda cpu: ((cpu.IsFlagSet('N') ^ cpu.IsFlagSet('V')) == False)
    self.JumpIfCondition(cpu, condition, args)
    
  def Jl(self, cpu, *args):
    """
    Jump if less than
    """
    condition =  lambda cpu: (cpu.IsFlagSet('N') ^ cpu.IsFlagSet('V')) == True
    self.JumpIfCondition(cpu, condition, args)
    
  def Jmp(self, cpu, *args):
    """
    Handle a direct jump instruction.
    """
    self.JumpIfCondition(cpu, lambda cpu: True == True, args)
    
  def Nop(self, cpu, *args):
    """
    Handle a NO-OPeration instruction.
    """
    return
  
  def Pop(self, cpu, *args):
    """
    Handle pop instructions.
    """
    mnemonic, dst_op = args 
    byte_mode = mnemonic.endswith('.B')
    result = self.ReadMem(cpu, self.GetRegister(cpu, "SP"), 2)
    self.SetRegister(cpu, "SP", self.GetRegister(cpu, "SP") + 2)
    
    if byte_mode:
      result = result & 0x00ff
      
    self.WriteDstOperand(cpu, dst_op, byte_mode, result)
    
  def Push(self, cpu, *args):
    """
    Handle push instructions.
    """
    mnemonic, src_op = args 
    byte_mode = mnemonic.endswith('.B')
    
    src_val = self.GetOpValue(cpu, src_op, byte_mode)
    
    self.SetRegister(cpu, "SP", self.GetRegister(cpu, "SP") - 2)
    
    if byte_mode:
      src_val = src_val &  0x00ff
      
    self.WriteDstOperand(cpu, "@SP", byte_mode, src_val)
    
  def Ret(self, cpu, *args):
    """
    Handle return instructions.
    """
    self.Pop(cpu, "POP.W", "PC")
    
  def Rla(self, cpu, *args):
    """
    Handle Rotate Left Arithmetically.
    """
    mnemonic, src_op = args 
    byte_mode = mnemonic.endswith('.B')

    new_mnemonic = "ADD"
    if byte_mode:
      new_mnemonic += ".B"
    else:
      new_mnemonic += ".W"
      
    self.Add(cpu, new_mnemonic, src_op, src_op)
    
  def Rlc(self, cpu, *args):
    """
    Rotate left through carry. (treating as a NOP right now).
    """
    mnemonic, src_op = args 
    byte_mode = mnemonic.endswith('.B')

    new_mnemonic = "ADDC"
    if byte_mode:
      new_mnemonic += ".B"
    else:
      new_mnemonic += ".W"
      
    self.Addc(cpu, new_mnemonic, src_op, src_op)
    
  def Rra(self, cpu, *args):
    """
    Rotate Right arithmetically (treating as a NOP right now).
    """
    mnemonic, src_op = args 
    byte_mode = mnemonic.endswith('.B')
    op1 = self.GetOpValue(cpu, src_op, byte_mode)
      
    c_flag = op1 & 1 == 1
    result = op1 >> 1
    
    self.UpdateFlagsForOperand(cpu, 'CZNV', byte_mode, src_op, result)
    # this always sets the C bit to zero  
    cpu.UpdateFlags("V", False)
    cpu.UpdateFlags("C", c_flag)
    
    if byte_mode == True:
      result = result & 0x00ff
    
    self.WriteDstOperand(cpu, src_op, byte_mode, result)
    
  def Rrc(self, cpu, *args):
    """
    Rotate right through c. (treating as a NOP right now).
    """
    mnemonic, src_op = args 
    byte_mode = mnemonic.endswith('.B')
    op1 = self.GetOpValue(cpu, src_op, byte_mode)
      
    c_flag = op1 & 1 == 1
    
    result = op1 >> 1
    
    if cpu.IsFlagSet('C') == True and byte_mode == True:
      result = result | 0x80
    elif cpu.IsFlagSet('C') == True:
      result = result | 0x8000
      
    self.UpdateFlagsForOperand(cpu, 'ZNV', byte_mode, src_op, result)
    cpu.UpdateFlags('C', c_flag)
    cpu.UpdateFlags('V', False)
    self.WriteDstOperand(cpu, src_op, byte_mode, result)
    
  def Sbc(self, cpu, *args):
    """
    Subtract not (C) instruction.
    """
    mnemonic, dst_op = args 
    byte_mode = mnemonic.endswith('.B')

    new_mnemonic = "SUB"
    if byte_mode:
      new_mnemonic += ".B"
    else:
      new_mnemonic += ".W"
      
    self.Subc(cpu, new_mnemonic, "#0", dst_op)
    
  def SetC(self, cpu, *args):
    """
    set carry (c) bit.
    """
    cpu.UpdateFlags('C', True)
    
  def SetN(self, cpu, *args):
    cpu.UpdateFlags('N', True)
    
  def SetZ(self, cpu, *args):
    cpu.UpdateFlags('Z', True)
    
  def Sub(self, cpu, *args):
    """
    Subtraction.
    """
    mnemonic, src_op, dst_op = args 
    byte_mode = mnemonic.endswith('.B')
    
    op1 = self.GetOpValue(cpu, src_op, byte_mode)
    op2 = self.GetOpValue(cpu, dst_op, byte_mode)
    
    # do the work of the operand
    result = op2 + (~op1 + 1)
    
    self.UpdateFlagsForOperand(cpu, 'CZNV', byte_mode, dst_op, result)

    if byte_mode:
      result = result & 0x00ff
      
    self.WriteDstOperand(cpu, dst_op, byte_mode, result)
    
  def Subc(self, cpu, *args):
    """
    Subtract and not c from src.
    """
    mnemonic, src_op, dst_op = args 
    byte_mode = mnemonic.endswith('.B')
    
    src_op_val = self.GetOpValue(cpu, src_op, byte_mode)
    dst_op_val = self.GetOpValue(cpu, dst_op, byte_mode)
    
    if cpu.IsFlagSet('C'):
      c = 1
    else:
      c = 0
    # do the work of the operand
    result = (dst_op_val + (~src_op_val + 1)) + c
    
    self.UpdateFlagsForOperand(cpu, 'CZNV', byte_mode, dst_op, result)
    if c == 1:
      cpu.UpdateFlags('C', False)
    else:
      cpu.UpdateFlags('C', True)

    if byte_mode:
      result = result & 0x00ff
      
    self.WriteDstOperand(cpu, dst_op, byte_mode, result)
    
  def Swpb(self, cpu, *args):
    """
    Handle swap bytes instruction.
    """
    mnemonic, dst_op = args
    
    op_val = self.GetOpValue(cpu, dst_op)
    result = ((op_val & 0xff) << 8) | ((op_val & 0xff00) >> 8)
    self.WriteDstOperand(cpu, dst_op, byte_mode=False, result=result)
   
  def Sxt(self, cpu, *args):
    """
    Handle sign extension instruction.
    """
    mnemonic, dst_op = args
    dst_op_val = self.GetOpValue(cpu, dst_op)
    
    if 0x80 & dst_op_val != 0:
      result = dst_op_val | 0xff80
    else:
      result = dst_op_val & 0x00ff
      
    self.UpdateFlagsForOperand(cpu, 'CZNV', False, dst_op, result)
    cpu.UpdateFlags('V', False)
    
    if result != 0:
      cpu.UpdateFlags('C', True)
      
    self.WriteDstOperand(cpu, dst_op, byte_mode=False, result=result)

  def Tst(self, cpu, *args):
    """
    Handle test instruction.
    """
    mnemonic, dst_op = args 
    byte_mode = mnemonic.endswith('.B')
    
    new_mnemonic = "CMP"
    if byte_mode:
      new_mnemonic += ".B"
   
    self.Cmp(cpu, new_mnemonic, "#0", dst_op) 
    
  def Xor(self, cpu, *args):
    """
    Handle xor instructions.
    """
    mnemonic, src_op, dst_op = args 
    byte_mode = mnemonic.endswith('.B')
    
    op1 = self.GetOpValue(cpu, src_op, byte_mode)
    op2 = self.GetOpValue(cpu, dst_op, byte_mode)
    
    # do the work of the operand
    result = op1 ^ op2
    
    self.UpdateFlagsForOperand(cpu, 'CZNV', byte_mode, dst_op, result)

    if byte_mode:
      result = result & 0x00ff
      
    self.WriteDstOperand(cpu, dst_op, byte_mode, result)