package com.rontologist.solium.romannumerals.server.number;

import com.rontologist.solium.romannumerals.server.numeral.Numeral;

import java.util.List;
import java.util.Collections;
import java.util.LinkedList;

public abstract class AbstractNumber<T extends Numeral> implements Number {
    private List<T> numerals = new LinkedList<T>();

    public List<T> getNumerals() {
        return Collections.unmodifiableList(numerals);
    }

    public void setNumerals(List<T> numerals) {
        this.numerals = numerals;
    }

    public String getRepresentation() {
        StringBuilder sBuilder = new StringBuilder();
        for (Numeral n : getNumerals()) {
            sBuilder.append(n.getRepresentation());
        }
        return sBuilder.toString();
    }
}