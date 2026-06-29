# template.py
from qiskit import QuantumCircuit, QuantumRegister, ClassicalRegister, transpile
from qiskit.circuit.library import RZGate, RZZGate, RXXGate, RYYGate
from qiskit.providers.basic_provider import BasicSimulator
from qiskit.visualization import plot_histogram
import numpy as np
import matplotlib.pyplot as plt
import math
from dicke_states import dicke_state
from fisher_yates import QuantumFisherYatesShuffle 
from qiskit import AncillaRegister

gamma = 0.2
beta  = 0.1
n_qubits = 3

trotter_steps = 100
T = 4
# try 2 or 4


qr = QuantumRegister(n_qubits)
cr = ClassicalRegister(n_qubits, name='cr')
qc = QuantumCircuit(qr, cr)

is_clique = False
is_k_clique = False
is_hamiltonianCycle = False
is_tsp = False

k_size = 3

# def get_dicke_state_amplitudes(n_qubits, k):
#     N = 2 ** n_qubits
#     num_valid_states = math.comb(n_qubits, k)
#     amplitude = 1.0 / math.sqrt(num_valid_states)
    
#     state_vector = np.zeros(N)
#     for i in range(N):
#         if bin(i).count('1') == k:
#             state_vector[i] = amplitude
#     return state_vector

if is_clique:
    for step in range(trotter_steps):
        for j in range(n_qubits):
            for k in range(n_qubits):
                if j != k:
                    qc.append(RXXGate(4 * gamma / trotter_steps), [j, k])
                    qc.append(RYYGate(4 * gamma / trotter_steps), [j, k])
elif is_k_clique:
    dicke_circ = dicke_state(n_qubits, k_size, draw=False, barrier=False)
    qc.compose(dicke_circ, inplace=True)
elif is_hamiltonianCycle or is_tsp:               
    # Dynamically determine N (cities) based on total allocated qubits
    elements_count = 0
    for i in range(1, n_qubits + 1):
        if i * math.ceil(math.log2(i)) == n_qubits:
            elements_count = i
            break
            
    if elements_count == 0:
        raise ValueError(f"Could not factor n_qubits={n_qubits} into N * ceil(log2(N))")
        
    element_length = math.ceil(math.log2(elements_count))
    input_values = list(range(elements_count))
    
    # 1. Classical Initialization: Load |0, 1, ..., N-1>
    for m, input_val in enumerate(input_values):
        input_val_in_binary = bin(input_val)[2:].zfill(element_length)
        for i, bit in enumerate(input_val_in_binary[::-1]):
            if bit == '1':
                qc.x(m * element_length + i)
                
    # 2. Set up ancillas and compose the Fisher-Yates Shuffler
    shuffler = QuantumFisherYatesShuffle(elements_count, element_length)
    num_ancillas = shuffler.num_qubits - (elements_count * element_length)
    
    anc = AncillaRegister(num_ancillas, 'anc')
    qc.add_register(anc)
    
    # Apply the shuffle specifically to the input register and the new ancillas
    qc.compose(shuffler, qubits=qr[:] + anc[:], inplace=True)
else:
    for i in range(n_qubits):
        qc.h(i)

# if is_clique:
#     for i in range(n_qubits):
#         qc.h(i)
# elif is_k_clique:
#     for i in range(k_size):
#         qc.h(i) 
# elif is_hamiltonianCycle or is_tsp:
#     qc.x(2)
#     qc.x(5)    
# else:
#     for i in range(n_qubits):
#         qc.h(i)

for step in range(trotter_steps):
# INSERT_RZZ_GATES_HERE
    

qc.measure(range(n_qubits), range(n_qubits))

simulator = BasicSimulator()

compiled_circuit = transpile(qc, simulator)
job = simulator.run(compiled_circuit, shots=1024)
result = job.result()
counts = result.get_counts()

print(counts)

