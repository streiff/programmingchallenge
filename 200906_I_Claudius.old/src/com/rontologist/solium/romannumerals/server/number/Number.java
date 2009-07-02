package com.rontologist.solium.romannumerals.server.number;

import com.rontologist.solium.romannumerals.server.numeral.Numeral;

import java.util.List;

public interface Number {
    int getValue();
    List<? extends Numeral> getNumerals();
    String getRepresentation();
}