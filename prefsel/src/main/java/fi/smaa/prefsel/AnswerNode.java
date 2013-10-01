package fi.smaa.prefsel;

import java.util.ArrayList;
import java.util.Collections;

public class AnswerNode implements Node<AnswerNode, QuestionNode> {

	private int answer;
	private QuestionNode[] children;

	public static final int NO_ANSWER = -1;

	public AnswerNode(int answer, Question[] remainingQuestions, TransitiveRelation prefs) {
		this.answer = answer;
		createChildren(remainingQuestions, prefs);
	}

	/**
	 * Build root node.
	 * 
	 * @param remainingQuestions
	 */
	public AnswerNode(Question[] allQuestions, int nrAlts) {
		this(NO_ANSWER, allQuestions, new TransitiveRelation(nrAlts));
	}

	
	private void createChildren(Question[] remainingQuestions, TransitiveRelation prefs) {
		children = new QuestionNode[remainingQuestions.length];
		for (int i=0;i<remainingQuestions.length;i++) {
			children[i] = new QuestionNode(remainingQuestions[i], cloneArrayWithoutOne(remainingQuestions, i), prefs);
		}		
	}

	private Question[] cloneArrayWithoutOne(Question[] remainingQuestions, int i) {
		ArrayList<Question> qs = new ArrayList<Question>();
		Collections.addAll(qs, remainingQuestions);
		qs.remove(i);
		return qs.toArray(new Question[0]);
	}

	public int getAnswer() {
		return answer;
	}

	public QuestionNode[] getChildren() {
		return children;
	}

	@Override
	public String toString() {
		return "a" + Integer.toString(answer);
	}
	
}