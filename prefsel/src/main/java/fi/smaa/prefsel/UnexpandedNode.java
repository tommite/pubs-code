package fi.smaa.prefsel;

public class UnexpandedNode implements AnswerNode {
	private int answer;
	
	public UnexpandedNode(int answer) {
		this.answer = answer;
	}

	public QuestionNode[] getChildren() {
		return new QuestionNode[0];
	}
	
	public String toString() {
		return "un";
	}

	public int getAnswer() {
		return answer;
	}
}
