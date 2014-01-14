package fi.smaa.prefsel;

import org.apache.commons.math3.linear.RealMatrix;

public class HDVFChoiceStrategy implements ChoiceStrategy {

	public QuestionNode[] nodesToExpand(QuestionNode[] children, RealMatrix measurements) {
		if (children.length < 1) {
			return children;
		}
		double[] hdvf = new double[children.length];
		RWrapper w = new RWrapper(children[0].getRelation(), measurements);
		double max = Double.MIN_VALUE;
		int idx = 0;
		for (int i=0;i<children.length;i++) {
			Question q = children[i].getQuestion();
			w.computeMetrics(q.getA1(), q.getA2());
			hdvf[i] = w.getHVDF();
			if (hdvf[i] > max) {
				idx = i;
				max = hdvf[i];
			}
		}
		
		return new QuestionNode[] {children[idx]};
	}
}
