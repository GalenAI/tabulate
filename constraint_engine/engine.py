import pandas as pd
import snap
import pickle
import zmq

def get_sim_graph():
    """
    Loads and returns similarity graph over all drug ids in BioSNAP dataset.
    Really slow so only do this once. Passed to get_interactions.

    :return: edge list of drug id similarity graph
    """
    return snap.LoadEdgeList(snap.TUNGraph, "drug-sim-int.tsv", 0, 1, "\t")

def get_drug_mappings():
    """
    Loads mappings from drug id to drug name and drug name to drug id.
    Only do this once. Passed to get_interactions.

    :return: (map from ids to names, map from names to ids)
    """
    with open('id_to_name.pickle', 'rb') as f:
        id_to_name = pickle.load(f)
    with open('name_to_id.pickle', 'rb') as f:
        name_to_id = pickle.load(f)
    return id_to_name, name_to_id

def get_interactions(drug_names, sim_graph, id_to_name, name_to_id):
    """
    Computes existing interactions in drug_names by querying subgraph
    of sim_graph.

    :param drug_ids: list of drug names
    :param sim_graph: similarity graph over all drug ids in BioSNAP
    :param id_to_name: mapping from drug ids to names
    :param name_to_id: mapping from names to drug ids

    :return: list of pairs of intersecting drug names
    """
    drug_ids = [name_to_id[name] for name in drug_names if name in name_to_id]
    sub_graph = sim_graph.GetSubGraph(drug_ids)
    return [(id_to_name[ei.GetSrcNId()], id_to_name[ei.GetDstNId()]) for ei in sub_graph.Edges()]

def test_intersections():
    """
    Tests for common interactions between drugs. Uses pytest to run.
    See https://www.goodrx.com/healthcare-access/medication-education/drug-interactions.
    """
    sg = get_sim_graph()
    id_to_name, name_to_id = get_drug_mappings()
    assert get_interactions(["Fluconazole", "Simvastatin"], sg, id_to_name, name_to_id) == [("Fluconazole", "Simvastatin")]
    assert get_interactions(["Ondansetron", "Dofetilide"], sg, id_to_name, name_to_id) == [("Dofetilide", "Ondansetron")]
    assert get_interactions(["Digoxin", "Amiodarone"], sg, id_to_name, name_to_id) == [("Digoxin", "Amiodarone")]


def get_patient_data():
    """
    Hardcoded patient data
    """
    return {
        "side": "left",
        "site": "abdomen",
        "allergies": ["Digoxin"],
    }


def get_mistakes(data, sim_graph, id_to_name, name_to_id):
    """
    Returns all possible mistakes given surgery data.

    :param data: surgery data with side, site, medications
    :param sim_graph: similarity graph over all drug ids in BioSNAP
    :param id_to_name: mapping from drug ids to names
    :param name_to_id: mapping from names to drug ids

    :return: list of mistakes with 4 possible types: side, site, allergies, interactions
    """
    mistakes = []
    patient = get_patient_data()

    # SIDE
    if data["side"] is not None and data["side"][0].lower() != patient["side"]:
        mistakes.append(["Wrong_side"])

    # SITE
    for site in data["sites"]:
        if site != patient["site"]:
            mistakes.append(["Wrong_site",
                            {"actual": patient["site"], "wrong": site}])

    # ALLERGIES
    drugs = [d.capitalize() for d in data["medications"]]
    allergies = patient["allergies"]
    mistakes.extend([["Drug_allergy", d] for d in drugs if d in allergies])

    # INTERACTIONS
    interactions = get_interactions(drugs, sim_graph, id_to_name, name_to_id)
    mistakes.extend([["Drug_to_drug", {"first": d1, "second": d2}] for (d1, d2) in interactions])

    return mistakes

def main():
    context = zmq.Context()
    in_socket = context.socket(zmq.SUB)
    in_socket.connect("ipc:///tmp/parsed")
    in_socket.subscribe("")

    out_socket = context.socket(zmq.PUB)
    out_socket.bind("ipc:///tmp/mistakes")

    sim_graph = get_sim_graph()
    id_to_name, name_to_id = get_drug_mappings()

    while True:
        data = in_socket.recv_json()
        print(f"[INFO] Received data {data}")
        mistakes = get_mistakes(data, sim_graph, id_to_name, name_to_id)
        print(f"[INFO] Sending {mistakes}")
        out_socket.send_json(mistakes)

if __name__ == '__main__':
    main()
