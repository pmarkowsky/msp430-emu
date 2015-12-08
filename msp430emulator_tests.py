"""
msp430emulator_tests.py -- unit tests for the msp430 emulator
"""
import unittest
import emulator

class TestCPUState(unittest.TestCase):
  """
  Ensure the emulator's CPU State class is able to update state and retrieve 
  state
  """
  def setUp(self):
    self.cpu_state = emulator.CPUState()
  
  def testInit(self):
    """
    Make sure that all registers are initialized to zero as is memory upon creation
    """
    regs = {'R0': 0x0000, 'R1': 0x0000, 'R2': 0x0000,  'R3':  0x0000,
            'R4': 0x0000, 'R5': 0x0000, 'R6': 0x0000,  'R7':  0x0000,
            'R8': 0x0000, 'R9': 0x0000, 'R10': 0x0000, 'R11': 0x0000, 
            'R12': 0x0000,'R13': 0x0000,'R14': 0x0000, 'R15': 0x0000,
            'PC': 0x0000, 'SP': 0x0000, 'SR': 0x0000,  'CG': 0x0000}
    
    for reg in regs:
      self.assertEqual(regs[reg], self.cpu_state.ReadReg(reg))
      
  def testReadWriteRegs(self):
    """
    
    """
    for reg in self.cpu_state.GetRegNames():
      # start by writing 0 to the register
      self.cpu_state.SetReg(reg, 0)
      self.assertEqual(self.cpu_state.ReadReg(reg), 0)
      # make sure we truncate values to 0 - 0xffff
      self.cpu_state.SetReg(reg, 0x1ffff)
      self.assertEqual(self.cpu_state.ReadReg(reg), 0xffff)
      self.cpu_state.SetReg(reg, 0x7fff)
      self.assertEqual(self.cpu_state.ReadReg(reg), 0x7fff)
      
  def testReadMemByte(self):
    """
    Ensure that we can read a byte from mem.
    """
    self.cpu_state.WriteMem(0x2400, "\x41")
    self.assertEqual(self.cpu_state.ReadMem(0x2400, 1), 0x41)
    
  def testReadMemShort(self):
    """
    Ensure that we're able to read a short from memory
    """
    self.cpu_state.WriteMem(0x3800, "\x41\x42")
    self.assertEqual(self.cpu_state.ReadMem(0x3800, 2), 0x4241)
    
  def testUpdateFlags(self):
    """
    Ensure that we can toggle flags on and off. Correctly updating the SR register.
    """
    for flag in ['C', 'Z', 'N', 'V']:
      self.cpu_state.SetReg('SR', 0x0000)
      self.cpu_state.UpdateFlags(flag, True)
      self.assertTrue(self.cpu_state.ReadReg('SR') & (0x1 << self.cpu_state.flag_mask[flag]))
      self.assertTrue(self.cpu_state.IsFlagSet(flag))
      self.cpu_state.UpdateFlags(flag, False)
      self.assertFalse(self.cpu_state.ReadReg('SR') & (0x1 << self.cpu_state.flag_mask[flag]))
      self.assertFalse(self.cpu_state.IsFlagSet(flag))

class TestMSP430InstructionEvaluator(unittest.TestCase):
  """
  test parsing of instructions and evaluation of instructions.
  """
  def setUp(self):
    self.cpu_state = emulator.CPUState()
    self.evaluator = emulator.MSP430InstructionEvaluator()
  
  def testAdd(self):
    """
    Test the add instruction. 
    """
    self.cpu_state.SetReg("R4", 0x1)
    self.cpu_state.SetReg("R5", 0x2)
    
    reg_reg_mnemonic = "ADD.W R4, R5"
    new_cpu = self.evaluator.EvaluateInstruction(reg_reg_mnemonic,
                                                 self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("R5"), 3)
    # make sure no flags were set.
    self.assertEqual(new_cpu.ReadReg("SR"), 0x0000)
    reg_reg_byte_mnemonic = "ADD.B R4, R5"
    self.cpu_state.SetReg("R4", 0x1)
    self.cpu_state.SetReg("R5", 0xfff2)
    self.cpu_state.SetReg("SR", 0x0)
    new_cpu = self.evaluator.EvaluateInstruction(reg_reg_byte_mnemonic,
                                                 self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("R5"), 0x00f3)
    # this sets the N flag in byte mode
    self.assertEqual(new_cpu.ReadReg("SR"), 0x0004)

    self.cpu_state.WriteMem(0x2400, "\x0a\xff")
    sym_reg_reg_mnemonic = "ADD.B @R4, R5"
    self.cpu_state.SetReg("R4", 0x2400)
    self.cpu_state.SetReg("R5", 0xff62)
    self.cpu_state.SetReg("SR", 0x0)
    
    new_cpu = self.evaluator.EvaluateInstruction(sym_reg_reg_mnemonic,
                                                 self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("R5"), 0x006c)
    self.assertEqual(new_cpu.ReadReg("SR"), 0x0000)

    self.cpu_state.WriteMem(0x2400, "\x0a\xff")
    self.cpu_state.SetReg("R5", 0xff62)
    self.cpu_state.SetReg("SR", 0x0)

    absolute_addr_reg_mnemonic = "ADD.B &2400, R5"
    new_cpu = self.evaluator.EvaluateInstruction(absolute_addr_reg_mnemonic,
                                                 self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("R5"), 0x006c)
    self.assertEqual(new_cpu.ReadReg("SR"), 0x0000)
    self.cpu_state.WriteMem(0x2400, "\x0a\xff")
    self.cpu_state.SetReg("R4", 0x2400 - 2)
    self.cpu_state.SetReg("R5", 0x00)
    self.cpu_state.SetReg("SR", 0x0)
    indexed_read_mnemonic = "ADD.W 2(R4), R5"
    new_cpu = self.evaluator.EvaluateInstruction(indexed_read_mnemonic,
                                                 self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("R5"), 0xff0a)
    self.assertEqual(new_cpu.ReadReg("SR"), 0x0104)
    
    
  def testMove(self):
    """
    Test move instructions.
    """
    self.cpu_state.SetReg("R5", 0x4242)
    new_cpu = self.evaluator.EvaluateInstruction("MOV.W R5, R7", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("R5"), 0x4242)
    self.assertEqual(new_cpu.ReadReg("R7"), 0x4242)
    self.assertEqual(new_cpu.ReadReg("SR"), 0x0000)
    self.cpu_state.SetReg("R7", 0x0)
    new_cpu = self.evaluator.EvaluateInstruction("MOV.B R5, R7", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("R5"), 0x4242)
    self.assertEqual(new_cpu.ReadReg("R7"), 0x0042)
    
  def testMoveAbsoluteAddress(self):
    """
    ensure we can move data to an absolute address.
    """
    self.cpu_state.SetReg("R5", 0xff24)
    new_cpu = self.evaluator.EvaluateInstruction("MOV R5, &2400", self.cpu_state)
    self.assertEqual(new_cpu.ReadMem(0x2400, 2), 0xff24)
  
  def testMoveIndexedAddress(self):
    """
    Ensure that we can write to an indexed address Correctly.
    """
    self.cpu_state.SetReg("R5", 0xff24)
    self.cpu_state.SetReg("R4", 0x2400)
    new_cpu = self.evaluator.EvaluateInstruction("MOV R5, 0(R4)", self.cpu_state)
    self.assertEqual(new_cpu.ReadMem(0x2400, 2), 0xff24)
    # test with an offset
    new_cpu = self.evaluator.EvaluateInstruction("MOV R5, 2(R4)", self.cpu_state)
    self.assertEqual(new_cpu.ReadMem(0x2402, 2), 0xff24)
    
    
  def testAdc(self):
    """
    Test Add carry flag.
    """
    # set the carry flag to 1
    self.cpu_state.UpdateFlags('C', True)
    self.cpu_state.SetReg("R4", 1)
    new_cpu = self.evaluator.EvaluateInstruction("ADC R4", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("R4"), 2)
    # make sure we reset flags to zero
    self.assertEqual(new_cpu.ReadReg('SR'), 0)
    # make sure that when the Carry flag is zero we jsut get the number back
    new_cpu.SetReg("R4", 1)
    new_cpu = self.evaluator.EvaluateInstruction("ADC R4", new_cpu)
    self.assertEqual(new_cpu.ReadReg("R4"), 1)
    self.assertEqual(new_cpu.ReadReg('SR'), 0)
    
  def testAddc(self):
    """
    Test add with carry
    """
    self.cpu_state.UpdateFlags('C', True)
    self.cpu_state.SetReg("R4", 1)
    self.cpu_state.SetReg("R5", 1)
    new_cpu = self.evaluator.EvaluateInstruction("ADDC R4, R5", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("R5"), 3)
    # make sure we reset flags to zero
    self.assertEqual(new_cpu.ReadReg('SR'), 0)
    
  def testAnd(self):
    """
    test logical and instructions.
    """
    self.cpu_state.SetReg("R4", 0)
    self.cpu_state.SetReg("R5", 1)
    new_cpu = self.evaluator.EvaluateInstruction("AND R4, R5", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("R5"), 0)
    # make sure we only set the Z flag
    self.assertEqual(new_cpu.ReadReg('SR'), 2)
    new_cpu.SetReg("R4", 1)
    new_cpu = self.evaluator.EvaluateInstruction("AND R4, R5", new_cpu)
    self.assertEqual(new_cpu.ReadReg("R5"), 0)
    # make sure we only set the Z flag
    self.assertEqual(new_cpu.ReadReg('SR'), 2)
    new_cpu.SetReg("R4", 1)
    new_cpu.SetReg("R5", 1)
    new_cpu = self.evaluator.EvaluateInstruction("AND R4, R5", new_cpu)
    self.assertEqual(new_cpu.ReadReg("R5"), 1)
    # make sure we reset flags to zero
    self.assertEqual(new_cpu.ReadReg('SR'), 0)

  def testBic(self):
    """
    test bit clear instructions.
    """
    self.cpu_state.SetReg("R5", 0x101)
    new_cpu = self.evaluator.EvaluateInstruction("BIC #100, R5", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("R5"), 1)
  
  def testBis(self):
    """
    test bit set instructions
    """
    self.cpu_state.SetReg("R5", 0x1)
    new_cpu = self.evaluator.EvaluateInstruction("BIS #101, R5", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("R5"), 0x101)
  
  def testBit(self):
    """
    test bit test instructions
    """
    self.cpu_state.SetReg("R5", 0x1)
    new_cpu = self.evaluator.EvaluateInstruction("BIT #01, R5", self.cpu_state)
    # make sure R5 isn't changed
    self.assertEqual(new_cpu.ReadReg("R5"), 0x01)
    # check flags.
    self.assertEqual(new_cpu.ReadReg("SR"), 0x0)
    new_cpu.SetReg("R5", 0)
    new_cpu = self.evaluator.EvaluateInstruction("BIT #01, R5", new_cpu)
    self.assertEqual(new_cpu.ReadReg("SR"), 0x2)
  
  def testBr(self):
    """
    test branch instructions.
    """
    new_cpu = self.evaluator.EvaluateInstruction("BR #4400", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("PC"), 0x4400)
    self.assertEqual(new_cpu.ReadReg("SR"), 0x0)
  
  def testCall(self):
    """
    test call instructions.
    """
    self.cpu_state.SetReg("SP", 0x4400)
    new_cpu = self.evaluator.EvaluateInstruction("CALL #5000", self.cpu_state)
    # check that we set pc correctly
    self.assertEqual(new_cpu.ReadReg("PC"), 0x5000)
    # check that we updated SP correctly (0x4400 - 2)
    self.assertEqual(new_cpu.ReadReg("SP"), 0x43fe)
    # check that we updated the stack memory correctly old PC + 2 (or in our case 2)
    self.assertEqual(new_cpu.ReadMem(new_cpu.ReadReg("SP"), 2), 0x0002)
  
  def testClr(self):
    """
    test clear instructions.
    """
    self.cpu_state.SetReg("R8", 0x4242)
    new_cpu = self.evaluator.EvaluateInstruction("CLR R8", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("R8"), 0)
    self.cpu_state.SetReg("R8", 0x4242)
    new_cpu = self.evaluator.EvaluateInstruction("CLR.B R8", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("R8"), 0x00)

  def testClrC(self):
    """
    test clear carry (CLRC) instructions
    """
    new_cpu = self.evaluator.EvaluateInstruction("CLRC", self.cpu_state)
    self.assertFalse(new_cpu.IsFlagSet('C'))
    self.cpu_state.UpdateFlags('C', True)
    new_cpu = self.evaluator.EvaluateInstruction("CLRC", self.cpu_state)
    self.assertFalse(new_cpu.IsFlagSet('C'))
  
  def testClrN(self):
    """
    test clear negative (CLRN) instructions.
    """
    new_cpu = self.evaluator.EvaluateInstruction("CLRN", self.cpu_state)
    self.assertFalse(new_cpu.IsFlagSet('N'))
    self.cpu_state.UpdateFlags('N', True)
    new_cpu = self.evaluator.EvaluateInstruction("CLRN", self.cpu_state)
    self.assertFalse(new_cpu.IsFlagSet('N'))
  
  def testClrZ(self):
    """
    test clear zero (CLRZ) instructions.
    """
    new_cpu = self.evaluator.EvaluateInstruction("CLRZ", self.cpu_state)
    self.assertFalse(new_cpu.IsFlagSet('Z'))
    self.cpu_state.UpdateFlags('Z', True)
    new_cpu = self.evaluator.EvaluateInstruction("CLRZ", self.cpu_state)
    self.assertFalse(new_cpu.IsFlagSet('Z'))
  
  def testCmp(self):
    """
    test compare (CMP) instructions.
    """
    self.cpu_state.SetReg("R5", 9)
    self.cpu_state.SetReg("R4", 9)
    
    # make sure we set carry flags and zero flag if the they're small & equal
    new_cpu = self.evaluator.EvaluateInstruction("CMP R4, R5", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("SR"), 0x3)
    # make sure we only set the N flag when
    self.cpu_state.SetReg("R4", 0x10)
    new_cpu = self.evaluator.EvaluateInstruction("CMP R5, R4", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("SR"), 0x1)
    # make sure we set the negative.
    new_cpu = self.evaluator.EvaluateInstruction("CMP R4, R5", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("SR"), 0x104)

  def testDADC(self):
    """
    test decimal add carry.
    """
    self.cpu_state.UpdateFlags("C", True)
    self.cpu_state.SetReg("R5", 0x10)
    new_cpu = self.evaluator.EvaluateInstruction("DADC R5", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("R5"), 0x11)
    self.cpu_state.UpdateFlags("C", False)
    new_cpu = self.evaluator.EvaluateInstruction("DADC R5", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("R5"), 0x10)
    
  
  def testDADD(self):
    """
    test decimal add. (Needs more tests)
    """
    self.cpu_state.SetReg("R4", 0x10)
    self.cpu_state.SetReg("R5", 0x11)
    new_cpu = self.evaluator.EvaluateInstruction("DADD R4, R5", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("R5"), 0x21)
    self.cpu_state.SetReg("R5", 0xf0)
    new_cpu = self.evaluator.EvaluateInstruction("DADD.B R4, R5", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("R5"), 0x60)
    #TODO fix flags.
    self.assertEqual(new_cpu.ReadReg("SR"), 0x0)
    new_cpu = self.evaluator.EvaluateInstruction("DADD R4, R5", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("R5"), 0x160)
    #TODO fix flags.
    self.assertEqual(new_cpu.ReadReg("SR"), 0x0)

  def testDINT(self):
    """
    test disable interrrupt instruction (TO DO).
    """
    pass
  
  def testEINT(self):
    """
    test enable interrupt instruction (TO DO).
    """
    pass
  
  def testInc(self):
    """
    Test inc and inc.b instructions.
    """
    self.cpu_state.SetReg("R4", 0x1)
    new_cpu = self.evaluator.EvaluateInstruction("INC R4", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("R4"), 2)
    self.assertEqual(new_cpu.ReadReg("SR"), 0)
    new_cpu = self.evaluator.EvaluateInstruction("INC.B R4", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("R4"), 2)
    self.assertEqual(new_cpu.ReadReg("SR"), 0)
  
  def testIncd(self):
    """
    Test incd and incd.b instrucions.
    """
    self.cpu_state.SetReg("R4", 0x1)
    new_cpu = self.evaluator.EvaluateInstruction("INCD R4", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("R4"), 3)
    self.assertEqual(new_cpu.ReadReg("SR"), 0)
    new_cpu = self.evaluator.EvaluateInstruction("INCD.B R4", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("R4"), 3)
    self.assertEqual(new_cpu.ReadReg("SR"), 0)
  
  def testInv(self):
    """
    Test inv and inv.b instructions.
    """
    self.cpu_state.SetReg("R4", 0x1)
    new_cpu = self.evaluator.EvaluateInstruction("INV R4", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("R4"), 0xfffe)
    self.assertEqual(new_cpu.ReadReg("SR"), 0x104)
    new_cpu = self.evaluator.EvaluateInstruction("INV.B R4", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("R4"), 0x00fe)
    self.assertEqual(new_cpu.ReadReg("SR"), 0x104)
  
  def testJC(self):
    """
    Test jump if carry instructions.
    """
    self.cpu_state.UpdateFlags("C", True)
    new_cpu = self.evaluator.EvaluateInstruction("JC #4400", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("PC"), 0x4400)
    self.cpu_state.UpdateFlags("C", False)
    new_cpu = self.evaluator.EvaluateInstruction("JC #4400", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("PC"), 0x0002)

  def testJz(self):
    """
    test jump if zero instructions.
    """
    self.cpu_state.UpdateFlags("Z", True)
    new_cpu = self.evaluator.EvaluateInstruction("JZ #4400", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("PC"), 0x4400)
    self.cpu_state.UpdateFlags("Z", False)
    new_cpu = self.evaluator.EvaluateInstruction("JZ #4400", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("PC"), 0x0002)
  
  def testJGE(self):
    """
    Test jump if greater than or equal to instructions.
    """
    new_cpu = self.evaluator.EvaluateInstruction("JGE #4400", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("PC"), 0x4400)
    self.cpu_state.UpdateFlags("N", True)
    new_cpu = self.evaluator.EvaluateInstruction("JGE #4400", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("PC"), 0x0002)
  
  def testJL(self):
    """
    Test jump if less than instructions.
    """
    self.cpu_state.UpdateFlags("N", True)
    new_cpu = self.evaluator.EvaluateInstruction("JL #4400", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("PC"), 0x4400)
    self.cpu_state.UpdateFlags("N", False)
    new_cpu = self.evaluator.EvaluateInstruction("JL #4400", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("PC"), 0x0002)
    
  def testJMP(self):
    """
    Test unconditional jump (jmp) instructions.
    """
    new_cpu = self.evaluator.EvaluateInstruction("JMP #4400", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("PC"), 0x4400)
  
  def testJN(self):
    """
    Test jump if negative (JN) instructions.
    """
    self.cpu_state.UpdateFlags("N", True)
    new_cpu = self.evaluator.EvaluateInstruction("JN #4400", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("PC"), 0x4400)
    self.cpu_state.UpdateFlags("N", False)
    new_cpu = self.evaluator.EvaluateInstruction("JN #4400", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("PC"), 0x0002)
  
  def testJNC(self):
    """
    Test jump if not carry (JNC) instructions.
    """
    new_cpu = self.evaluator.EvaluateInstruction("JNC #4400", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("PC"), 0x4400)
    self.cpu_state.UpdateFlags("C", True)
    new_cpu = self.evaluator.EvaluateInstruction("JNC #4400", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("PC"), 0x0002)
  
  def testJNZ(self):
    """
    Test jump if not zero (JNZ) instructions.
    """
    new_cpu = self.evaluator.EvaluateInstruction("JNZ #4400", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("PC"), 0x4400)
    self.cpu_state.UpdateFlags("Z", True)
    new_cpu = self.evaluator.EvaluateInstruction("JNZ #4400", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("PC"), 0x0002)
    
  def testPop(self):
    """
    Test pop instructions.
    """
    self.cpu_state.SetReg("SP", 0x2400)
    self.cpu_state.WriteMem(0x2400, "\x42\x42")
    new_cpu = self.evaluator.EvaluateInstruction("POP R4", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("SP"), 0x2402)
    self.assertEqual(new_cpu.ReadReg("R4"), 0x4242)
  
  def testPush(self):
    """
    Test push instructions.
    """
    self.cpu_state.SetReg("SP", 0x2400)
    self.cpu_state.SetReg("R4", 0x4242)
    new_cpu = self.evaluator.EvaluateInstruction("PUSH R4", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("SP"), 0x23fe)
    self.assertEqual(new_cpu.ReadMem(0x23fe, 2), 0x4242)
  
  def testRet(self):
    """
    Test return (RET) instructions.
    """
    self.cpu_state.SetReg("SP", 0x23fe)
    self.cpu_state.WriteMem(0x23fe, "\x42\x42")
    new_cpu = self.evaluator.EvaluateInstruction("RET", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("PC"), 0x4242)
    self.assertEqual(new_cpu.ReadReg("SP"), 0x2400)
    
  
  def testReti(self):
    """
    Test return (RETI) instructions. (TO DO)
    """
    pass
  
  def testRLA(self):
    """
    Test Rotate left arithmetically RLA instrucitons.
    """
    self.cpu_state.SetReg("R4", 4)
    new_cpu = self.evaluator.EvaluateInstruction("RLA R4", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("R4"), 8)
    new_cpu = self.evaluator.EvaluateInstruction("RLA.B R4", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("R4"), 8)

  
  def testRLC(self):
    """
    Test Rotate left through carry instructions.
    """
    self.cpu_state.SetReg("R4", 0xfff0)
    new_cpu = self.evaluator.EvaluateInstruction("RLC R4", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("R4"), 0xffe0)
    self.assertEqual(new_cpu.ReadReg("SR"), 0x5)
    new_cpu = self.evaluator.EvaluateInstruction("RLC.B R4", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("R4"), 0x00e0)
    self.assertEqual(new_cpu.ReadReg("SR"), 0x5)
  
  def testRRA(self):
    """
    Test rotate right arithmetically (RRA) instruction.
    """
    self.cpu_state.SetReg("R4", 0x00ff)
    new_cpu = self.evaluator.EvaluateInstruction("RRA R4", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("R4"), 0x007f)
    self.assertEqual(new_cpu.ReadReg("SR"), 0x1)
    self.cpu_state.SetReg("R4", 0xffff)
    new_cpu = self.evaluator.EvaluateInstruction("RRA.B R4", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("R4"), 0x007f)
    self.assertEqual(new_cpu.ReadReg("SR"), 0x1)
  
  def testRRC(self):
    """
    Test rotate right through carry.
    """
    self.cpu_state.SetReg("R4", 0x00ff)
    new_cpu = self.evaluator.EvaluateInstruction("RRA R4", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("R4"), 0x007f)
    self.assertEqual(new_cpu.ReadReg("SR"), 0x1)
    self.cpu_state.SetReg("R4", 0xffff)
    new_cpu = self.evaluator.EvaluateInstruction("RRA.B R4", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("R4"), 0x007f)
    self.assertEqual(new_cpu.ReadReg("SR"), 0x1)
  
  def testSBC(self):
    """
    Test SBC instructions.
    """
    self.cpu_state.UpdateFlags('C', True)
    new_cpu = self.evaluator.EvaluateInstruction("SBC R4", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("R4"), 1)
    self.assertEqual(new_cpu.ReadReg("SR"), 0)
  
  def testSetC(self):
    """
    Test Set Carry Flag instructions. 
    """
    new_cpu = self.evaluator.EvaluateInstruction("SETC", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("SR"), 0x1)
  
  def testSetN(self):
    """
    Test Set Carry Flag instructions. 
    """
    new_cpu = self.evaluator.EvaluateInstruction("SETN", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("SR"), 0x4)
  
  def testSetZ(self):
    """
    Test Set Zero Flag instructions.
    """
    new_cpu = self.evaluator.EvaluateInstruction("SETZ", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("SR"), 0x2)
  
  def testSub(self):
    """
    Test subtraction (SUB) instructions.
    """
    self.cpu_state.SetReg("R5", 9)
    self.cpu_state.SetReg("R4", 9)
    new_cpu = self.evaluator.EvaluateInstruction("SUB R5, R4", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("R4"), 0)
    self.assertEqual(new_cpu.ReadReg("SR"), 2)
  
  def testSubC(self):
    """
    Test subtract with carry (SUBC).
    """
    self.cpu_state.SetReg("R4", 0x00a0)
    self.cpu_state.UpdateFlags("C", True)
    new_cpu = self.evaluator.EvaluateInstruction("SUBC #4, R4", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("R4"), 157)
    self.assertEqual(new_cpu.ReadReg("SR"), 0x0)
  
  def testSWPB(self):
    """
    Test swap byte (SWPB) instructions.
    """
    self.cpu_state.SetReg("R4", 0xaabb)
    new_cpu = self.evaluator.EvaluateInstruction("SWPB R4", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("R4"), 0xbbaa)
    # ensure we didn't set any flags.
    self.assertEqual(new_cpu.ReadReg("SR"), 0x0000)
  
  def testSXT(self):
    """
    Test sign extension.
    """
    self.cpu_state.SetReg("R4", 0x7f)
    new_cpu = self.evaluator.EvaluateInstruction("SXT R4", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("R4"), 0x7f)
    self.assertEqual(new_cpu.ReadReg("SR"), 0x1)
    self.cpu_state.SetReg("R4", 0x80)
    new_cpu = self.evaluator.EvaluateInstruction("SXT R4", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("R4"), 0xff80)
    self.assertEqual(new_cpu.ReadReg("SR"), 5)
    
  def testTST(self):
    """
    Test test for zero instructions.
    """
    self.cpu_state.SetReg("R4", 0x1)
    new_cpu = self.evaluator.EvaluateInstruction("TST R4", self.cpu_state)
    self.assertEqual(new_cpu.IsFlagSet('C'), True)
    self.assertEqual(new_cpu.IsFlagSet('Z'), False)
    self.assertEqual(new_cpu.IsFlagSet('N'), False)
    self.assertEqual(new_cpu.IsFlagSet('V'), False)
  
  def testXor(self):
    """
    Test XOR and XOR.B instructions.
    """
    self.cpu_state.SetReg("R4", 0xff00)
    new_cpu = self.evaluator.EvaluateInstruction("XOR R4, R4", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("R4"), 0x0000)
    self.assertEqual(new_cpu.ReadReg("SR"), 0x102)
    self.cpu_state.SetReg("R5", 0x00ff)
    new_cpu = self.evaluator.EvaluateInstruction("XOR R4, R5", self.cpu_state)
    self.assertEqual(new_cpu.ReadReg("R5"), 0xFFFF)
    self.assertEqual(new_cpu.ReadReg("SR"), 0x104)

  def testSetFlagsFromMnemonic(self):
    """
    Ensure that we can update the status register appropriately.
    """
    # test that we set flags correctly
    self.cpu_state.SetReg("R4", 0x1)
    self.cpu_state.SetReg("R5", 0xffff)
    new_cpu = self.evaluator.EvaluateInstruction("ADD.W R4, R5", self.cpu_state)
    self.assertTrue(new_cpu.IsFlagSet('C'))
    self.assertFalse(new_cpu.IsFlagSet('N'))
    self.assertTrue(new_cpu.IsFlagSet('Z'))
    # need to fix 
    self.assertTrue(new_cpu.IsFlagSet('V'))
 
  
if __name__ == '__main__':
  unittest.main()