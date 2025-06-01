import dimod
import dwave.inspector
import minorminer
from dimod import BinaryQuadraticModel
from dwave.system import EmbeddingComposite, DWaveSampler


def run_on_dwave(Q: dict, label: str, chainstrength: float, numruns: int):
    sampler = EmbeddingComposite(DWaveSampler())
    response = sampler.sample_qubo(Q,
                                   chain_strength=chainstrength,
                                   num_reads=numruns,
                                   label=label)

    return response


def show_inspector(response):
    dwave.inspector.show(response)


def get_minor_embedding(Q: dict):
    bqm = BinaryQuadraticModel.from_qubo(Q)
    source_edgelist = list(bqm.quadratic) + [(v, v) for v in bqm.linear]

    sampler = DWaveSampler()
    target_structure = dimod.child_structure_dfs(sampler)
    __, target_edgelist, target_adjacency = target_structure

    embedding = minorminer.find_embedding(source_edgelist, target_edgelist)
    return embedding
