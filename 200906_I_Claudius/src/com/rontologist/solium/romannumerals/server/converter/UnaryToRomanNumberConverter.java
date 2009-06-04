package com.rontologist.solium.romannumerals.server.converter;

import com.rontologist.solium.romannumerals.server.number.UnaryNumber;
import com.rontologist.solium.romannumerals.server.number.UnaryNumber;
import com.rontologist.solium.romannumerals.server.number.RomanNumber;
import com.rontologist.solium.romannumerals.server.numeral.NumeralFactory;
import com.rontologist.solium.romannumerals.server.numeral.NumeralFactoryFactory;
import com.rontologist.solium.romannumerals.server.numeral.roman.RomanNumeral;
import com.rontologist.solium.romannumerals.server.numeral.unary.UnaryNumeral;

import java.util.List;
import java.util.LinkedList;

public class UnaryToRomanNumberConverter implements NumberConverter<UnaryNumber, RomanNumber> {

    public RomanNumber convert(UnaryNumber number) {
        NumeralFactory<RomanNumeral> factory = NumeralFactoryFactory.getInstance().createFactory(RomanNumeral.class);

        List<UnaryNumeral> unaryNumerals = new LinkedList<UnaryNumeral>(number.getNumerals());
        List<RomanNumeral> romanNumerals = new LinkedList<RomanNumeral>();

        while (!unaryNumerals.isEmpty()) {
            // convert as much as possible
            for (int i = unaryNumerals.size(); i > 0; --i) {
                List<UnaryNumeral> numeralAttemptList = unaryNumerals.subList(0, i);
                UnaryNumber numberAttempt = new UnaryNumber();
                numberAttempt.setNumerals(numeralAttemptList);

                RomanNumeral romanNumeral = factory.getNumeral(numberAttempt.getValue());

                if (romanNumeral != null) {
                    romanNumerals.add(romanNumeral);
                    unaryNumerals = unaryNumerals.subList(i, unaryNumerals.size());
                    break;
                }
            }
        }

        return new RomanNumber(romanNumerals);
    }
}