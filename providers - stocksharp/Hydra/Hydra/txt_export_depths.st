$depths:
{
	depth|
	$depth.LastChangeTime; format="HH:mm:ss"$

	$depth
	{
		quote|
		$quote.Price$, $quote.Volume$, $quote.OrderDirection$$\n$
	}$
}$