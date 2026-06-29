# Source: link 

import numpy as np
from qiskit import QuantumCircuit, QuantumRegister, AncillaRegister

class QuantumFisherYatesShuffle(QuantumCircuit):
    def __init__(self, elements_count: int, element_length: int, name: str = 'Shuffler') -> None:
        self.elements_count = elements_count
        self.element_length = element_length
        self.index_length = len(bin(self.elements_count)[2:])
        
        # Define registers
        input_qr = QuantumRegister(self.elements_count * self.element_length, 'in')
        index_qr = AncillaRegister(self.elements_count * self.index_length, 'idx')
        choice_qr = AncillaRegister(max(self.elements_count, self.element_length), 'ch')
        
        super().__init__(input_qr, index_qr, choice_qr, name=name)
        
        self._build_circuit(input_qr, index_qr, choice_qr)

    def _build_circuit(self, input_qr, index_qr, choice_qr):
        # 1. Initialization: Set index register to |0, 1, ..., n-1>
        for m in range(self.elements_count):
            m_in_binary = bin(m)[2:].zfill(self.index_length)
            for i, bit in enumerate(m_in_binary[::-1]):
                if bit == '1':
                    self.x(index_qr[m * self.index_length + i])
                    
        for k in range(1, self.elements_count):
            # 2a. Prepare choice (W-state generation)
            self.x(choice_qr[0])
            theta = 2 * np.arccos(1 / np.sqrt(k + 1))
            self.ry(theta, choice_qr[1])
            
            for l in range(2, k + 1):
                theta = 2 * np.arccos(1 / np.sqrt(k - l + 2))
                self.cry(theta, choice_qr[l - 1], choice_qr[l])
                
            for l in range(k):
                self.cx(choice_qr[l + 1], choice_qr[l])
            self.barrier()
            
            # 2b. Selected Swap
            for i in range(k):
                for j in range(self.index_length):
                    self.cswap(choice_qr[i], 
                               index_qr[i * self.index_length + j], 
                               index_qr[k * self.index_length + j])
            for i in range(k):
                for j in range(self.element_length):
                    self.cswap(choice_qr[i], 
                               input_qr[i * self.element_length + j], 
                               input_qr[k * self.element_length + j])
            self.barrier()
            
            # 2d. Resetting choice register
            k_in_binary = bin(k)[2:].zfill(self.index_length)
            for i in range(k + 1):
                self.mcx(index_qr[i * self.index_length : (i + 1) * self.index_length], 
                         choice_qr[i], 
                         ctrl_state=k_in_binary)
            self.barrier()
            
        # Phase 3 (Disentangling index from input) is omitted here for base Qiskit compatibility.
        for m in range(self.elements_count):
            for i in range(min(self.index_length, self.element_length)):
                self.cx(input_qr[m * self.element_length + i], 
                        index_qr[m * self.index_length + i])