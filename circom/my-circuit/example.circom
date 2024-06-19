pragma circom 2.1.9;

template Multiplier(n) {
    // signals are always private unless declared otherwise
    signal input a;
    signal input b;
    signal output c;

    // the circom language also has lists
    signal int[n];

    int[0] <== a*a + b;
    // the circom language also has loops
    for (var i=1; i<n; i++) {
    int[i] <== int[i-1]*int[i-1] + b;
    }

    c <== int[n-1];
}

// this marks that the signal b is public and all outputs are public
component main {public [b]} = Multiplier(100);
