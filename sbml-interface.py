#!/usr/bin/env python
from optparse import OptionParser
import libsbml as sbml
import os
HOME = os.path.expanduser("~")

def main(opts):
    doc = sbml.readSBMLFromFile(opts.model.replace("~", HOME))
    model = doc.getModel()
    if opts.description:
        print(model.getName())
    if opts.species:
        numSpecies = len(model.getListOfSpecies())
        for i in range(numSpecies):
            print(model.getSpecies(i).getName())
    if opts.reactions:
        numReactions = len(model.getListOfReactions())
        for i in range(numReactions):
            rxid = model.getReaction(i).getId()
            print(model.getReaction(rxid).getKineticLaw().getFormula())


if __name__ == '__main__':
    parser = OptionParser()
    parser.add_option("--model", type=str,help="Path to model SBML or zip file")
    parser.add_option("--description", action="store_true",default=False,help="Get name of model")
    parser.add_option("--species", action="store_true",help="Get list of species")
    parser.add_option("--reactions", action="store_true",help="Get list of reactions")
    opts, vals = parser.parse_args()
    main(opts)

