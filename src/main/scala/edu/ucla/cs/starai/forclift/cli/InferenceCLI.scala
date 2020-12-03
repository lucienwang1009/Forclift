/*
 * Copyright 2016 Guy Van den Broeck and Wannes Meert (UCLA and KU Leuven)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/*
 * Copyright (C) 2015 Guy Van den Broeck (guy.vandenbroeck@cs.kuleuven.be)
 *                    Wannes Meert (wannes.meert@cs.kuleuven.be)
 *
 * This file is part of WFOMC (http://dtai.cs.kuleuven.be/ml/systems/wfomc).
 *
 * WFOMC is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * WFOMC is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with WFOMC.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

package edu.ucla.cs.starai.forclift.cli

import org.clapper.argot.ArgotConverters._
import org.clapper.argot.ArgotParser
import edu.ucla.cs.starai.forclift.Atom
import edu.ucla.cs.starai.forclift.inference.WeightedCNF
import edu.ucla.cs.starai.forclift.compiler.Compiler
import edu.ucla.cs.starai.forclift.languages.StatRelModel
import edu.ucla.cs.starai.forclift.languages.ModelParser
import edu.ucla.cs.starai.forclift.util.Timer
import edu.ucla.cs.starai.forclift.inference.PartitionFunctionExact

/**
 * Handle all inference logic for CLI
 */
class InferenceCLI(
    argumentParser: ArgotParser, 
    debugCLI: DebugCLI,
    inputCLI: InputCLI) {
  
  /* INFERENCE FLAGS */
  
  val zFlag = argumentParser.flag[Boolean](
    List("z"),
    "Compute the weighted model count/partition function.")
  def z = zFlag.value.getOrElse(false)
  
  def hasQuery = inputCLI.hasQuery

  val fokc = true
  
  def runInference() {
    // make sure the model is parsed before inference timing starts
    inputCLI.wcnfModel
    inputCLI.queryOpt
    println("Starting to run inference")
    Timer{
      if(z) runPartitionFunctionInference()
    }("Inference took "+_+" ms")
  }
  
  def runPartitionFunctionInference(){
    // Only compute partition function
    println(s"Computing weighted model count/partition function Z")
    if (fokc) {
      val algo = new PartitionFunctionExact(debugCLI.verbose)
      algo.computePartitionFunction(inputCLI.wcnfModel)
    }
  }
  
}
