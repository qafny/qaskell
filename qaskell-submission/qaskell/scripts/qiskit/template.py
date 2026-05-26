# template.py
from qiskit import QuantumCircuit, QuantumRegister, ClassicalRegister, transpile
from qiskit.circuit.library import RZGate, RZZGate, RXXGate, RYYGate
from qiskit.providers.basic_provider import BasicSimulator
from qiskit.visualization import plot_histogram
import numpy as np
import matplotlib.pyplot as plt
import math
from dicke_states import dicke_state

gamma = 0.5
beta  = 0.1
n_qubits = 3

trotter_steps = 10
T = 1.0

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
# elif is_k_clique:
#     dicke_circ = dicke_state(n_qubits, k_size, draw=False, barrier=False)
#     qc.compose(dicke_circ, inplace=True)
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

