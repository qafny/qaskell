# template.py
from qiskit import QuantumCircuit, QuantumRegister, ClassicalRegister, transpile
from qiskit.circuit.library import RZGate, RZZGate, RXXGate, RYYGate
from qiskit.providers.basic_provider import BasicSimulator
from qiskit.visualization import plot_histogram
import numpy as np
import matplotlib.pyplot as plt

gamma = 0.5
beta  = 0.1
n_qubits = 3

trotter_steps = 10
T = 1.0

qr = QuantumRegister(n_qubits)
cr = ClassicalRegister(n_qubits, name='cr')
qc = QuantumCircuit(qr, cr)

is_clique = False

if is_clique:
    for step in range(trotter_steps):
        for j in range(n_qubits):
            for k in range(n_qubits):
                if j != k:
                    qc.append(RXXGate(4 * gamma / trotter_steps), [j, k])
                    qc.append(RYYGate(4 * gamma / trotter_steps), [j, k])
else:
    for i in range(n_qubits):
        qc.h(i)

for step in range(trotter_steps):
# INSERT_RZZ_GATES_HERE

    for i in range(n_qubits):
        qc.rx(2 * beta, i)

qc.measure(range(n_qubits), range(n_qubits))

simulator = BasicSimulator()

compiled_circuit = transpile(qc, simulator)
job = simulator.run(compiled_circuit, shots=1024)
result = job.result()
counts = result.get_counts()

print(result.get_counts())