from __future__ import annotations

import hashlib
import sys
from pathlib import Path

import pytest

EVAL_DIR = Path(__file__).resolve().parents[2]
if str(EVAL_DIR) not in sys.path:
    sys.path.insert(0, str(EVAL_DIR))

from gen.defects import generate_audit_task
from gen.generate import dump_json, generate_task


SNAPSHOTS = {
    "journalize": {
        "cash_sale": "a96e5814f9cf8c1916f11cafca5b3225848d31be92ae1766f181bf65b70279f9",
        "credit_trade": "5c9a91490a732b864575ae736e17a81687cc149f9fb9a8db95de24c494a3b6dd",
        "purchase": "616ff9b133fdef5e0696f916368f79ce31e81a72523cab3f6531efc1ab99b38e",
        "payroll": "09489666adec592f77c02b26ef993d80651aac7fc35582a28317a5540edbe327",
        "accrual": "ee9580460aa7e55931443886f42ee4d52be04e4938e8b917839c232efa37baaf",
        "tax": "6559b2a6b094d556a0d44d512ca501eb727ef1a5878922ad9ab588017d5c0f46",
        "fixed_asset": "b0b53d0e93f5faee107b12efce4bff08908db2b62d5457f468ccb80e71519547",
        "mixed": "9628a426c3be1d2c6a18123eec0646b4c127fa447867918232c991a51a32da05",
    },
    "audit": {
        "cash_sale": "2983ae933f6ed1ee23a99e893a63c7b41e94018cd595288758bba806c107c582",
        "credit_trade": "c3e4d785701d431fe83a45c67fd2f71632b50a779be43771fcf48c33ce72a580",
        "purchase": "2b15e39f5330b2e9972448509e23bd75d0b8635af05aedc8c5fc4094dfd06c90",
        "payroll": "0fbbede7c8dca8d85d215a61a7cf63f7bdfaf17c6503c173e4ff9a33fe158b59",
        "accrual": "cfdbbc34ecac435171cb24ca54cde3ba62881506490ad098beda8664085e3451",
        "tax": "5d39afa5fc856a2ca03f99f85bef42ca9642bd106a83c9a77d7845ff70aa8c8e",
        "fixed_asset": "c6d7fb2b115aca8a126fff7ce12d11bf6d240c6827085ab0464898ef852ee000",
        "mixed": "3e2c7160ecdacc9674ae1b26f1820ef9c9ab998bd55d72146662fbe8ca38fadf",
    },
}


@pytest.mark.parametrize(
    ("kind", "template", "expected"),
    [
        (kind, template, expected)
        for kind, templates in SNAPSHOTS.items()
        for template, expected in templates.items()
    ],
)
def test_existing_generated_task_bytes_are_frozen(
    kind: str,
    template: str,
    expected: str,
) -> None:
    """Protect experiment-1 task bytes from generator refactors."""
    if kind == "journalize":
        task = generate_task(seed=0, count=5, template=template)
    else:
        task = generate_audit_task(seed=0, count=5, defects=2, template=template)

    actual = hashlib.sha256(dump_json(task).encode("utf-8")).hexdigest()
    assert actual == expected
