package com.rontologist.solium.romannumerals.server.converter;

import com.rontologist.solium.romannumerals.server.number.UnaryNumber;
import com.rontologist.solium.romannumerals.server.number.RomanNumber;
import com.rontologist.solium.romannumerals.server.numeral.NumeralFactory;
import com.rontologist.solium.romannumerals.server.numeral.NumeralFactoryFactory;
import com.rontologist.solium.romannumerals.server.numeral.roman.RomanNumeral;

import java.util.List;
import java.util.LinkedList;

public class UnaryToRomanNumberConverter implements NumberConverter<UnaryNumber, RomanNumber> {

    public RomanNumber convert(UnaryNumber number) {
        NumeralFactory<RomanNumeral> factory = NumeralFactoryFactory.getInstance().createFactory(RomanNumeral.class);

        List<RomanNumeral> romanNumerals = new LinkedList<RomanNumeral>();

        while (number.getValue() > 0) {
            UnaryNumber numberAttempt = new UnaryNumber(number.getNumerals());
            while (numberAttempt.getValue() > 0) {
                RomanNumeral romanNumeral = factory.getNumeral(numberAttempt.getValue());
                if (romanNumeral != null) {
                    romanNumerals.add(romanNumeral);
                    number = number.subtract(numberAttempt);
                    break;
                }
                numberAttempt = numberAttempt.subtract(UnaryNumber.ONE);
            }
        }

        return new RomanNumber(romanNumerals);
    }
}
