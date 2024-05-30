pragma circom 2.1.9;

include "../circomlib/circuits/poseidon.circom";

template ProveKnowledgeOfPreImage(n){
    signal input in[2];
    signal output out[n];

    component poseidon = Poseidon(2);

    poseidon.inputs[0] <== in[0];
    poseidon.inputs[1] <== in[1];

    for (var i = 0; i < n; i++) {
        out[i] <== poseidon.out;
    }
}

component main = ProveKnowledgeOfPreImage(10);