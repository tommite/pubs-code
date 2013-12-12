package fi.smaa.prefsel;

import org.apache.commons.math3.linear.RealMatrix;

public interface ChoiceStrategy {
	public QuestionNode[] nodesToExpand(QuestionNode[] children, RealMatrix measurements);
}
