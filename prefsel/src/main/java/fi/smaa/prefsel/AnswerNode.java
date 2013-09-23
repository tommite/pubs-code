package fi.smaa.prefsel;

import java.util.ArrayList;
import java.util.Collections;

public class AnswerNode {
	
	private int answer;
	private QuestionNode[] children;
	
	public static final int NO_ANSWER = -1;

	public AnswerNode(Question[] remainingQuestions, int answer) {
		this.answer = answer;
		createChildren(remainingQuestions);
	}
	
	private void createChildren(Question[] remainingQuestions) {
		children = new QuestionNode[remainingQuestions.length];
		for (int i=0;i<remainingQuestions.length;i++) {
			children[i] = new QuestionNode(remainingQuestions[i], cloneArrayWithoutOne(remainingQuestions, i));
		}		
	}

	private Question[] cloneArrayWithoutOne(Question[] remainingQuestions, int i) {
		ArrayList<Question> qs = new ArrayList<Question>();
		Collections.addAll(qs, remainingQuestions);
		qs.remove(i);
		return qs.toArray(new Question[0]);
	}

	/**
	 * Build a root node (no answer)
	 * 
	 * @param questions
	 */
	public AnswerNode(Question[] questions) {
		answer = NO_ANSWER;
		createChildren(questions);
	}

	public int getAnswer() {
		return answer;
	}
	
	public QuestionNode[] getChildren() {
		return children;
	}
}
