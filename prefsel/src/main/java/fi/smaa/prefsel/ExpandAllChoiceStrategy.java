package fi.smaa.prefsel;

import org.apache.commons.math3.linear.RealMatrix;

public class ExpandAllChoiceStrategy implements ChoiceStrategy {

	public QuestionNode[] nodesToExpand(QuestionNode[] children, RealMatrix measurements) {
		return children;
	}

}
